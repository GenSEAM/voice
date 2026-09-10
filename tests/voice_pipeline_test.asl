(module asl-voice/tests/voice-pipeline-test
  :d "Unit verification suite for unified voice pipeline, noise gating, speculative Voice ID, barge-in, paraphrasing, and TTS formatting"
  :x [run-tests
      test-noise-only-gating-and-skipping
      test-speech-onset-and-speculative-voice-id-success
      test-speculative-voice-id-rejection-and-discard
      test-conversational-barge-in-interruption
      test-local-paraphrasing-and-fast-path
      test-spoken-tts-formatting
      test-terminal-tui-hud-sparklines]
  :i [(audio_frame :a af)
      (vad :a vad)
      (speaker_match :a sm)
      (voice_pipeline :a vp)])

(df make-constant-samples [(n Int64) (val Float)] -> (List Float)
  :d "Generates a list of n identical float samples"
  (if (<= n 0)
    (list)
    (list-cons val (make-constant-samples (- n 1) val))))

(df make-test-speaker [(speaker-id Str) (threshold Float)] -> sm/SpeakerProfile
  :d "Constructs an enrolled speaker profile with uniform 16-channel embedding"
  (let [(emb (make-constant-samples 16 0.25))]
    (sm/make-speaker-profile speaker-id "Alice" emb threshold)))

(df make-pipeline-test-frame [(id Str) (db Float) (is-speech Bool)] -> af/AudioFrame
  :d "Constructs an audio frame fixture for pipeline testing"
  (af/AudioFrame
    :frame-id id
    :samples-count 160
    :duration-ms 10
    :rms-energy 0.05
    :db-level db
    :is-speech is-speech
    :timestamp-ms 1000))

(df test-noise-only-gating-and-skipping [] -> Bool
  :d "Verifies ambient noise frames below speech threshold are discarded without activating ASR"
  (let [(speaker (make-test-speaker "spk-01" 0.82))
        (cfg (vp/make-voice-pipeline-config speaker "terminal-tui"))
        (st0 (vp/make-initial-voice-pipeline-state))
        (noise-frame (make-pipeline-test-frame "f-noise-1" -62.0 false))
        (samples (make-constant-samples 160 0.001))
        (res (vp/pipeline-step-frame cfg st0 noise-frame samples))
        (st1 (.-next-state res))]
    (assert (= (.-action-kind res) "noise-skipped") "Quiet noise frame must be tagged noise-skipped")
    (assert (= (.-noise-frames-skipped st1) 1) "Noise frame skip counter must increment to 1")
    (assert (= (.-speech-frames-collected st1) 0) "Speech frames collected must remain zero")
    (assert (not (.-is-utterance-active st1)) "Utterance must not be active on ambient noise")
    (assert (= (.-transcript res) "") "Transcript must remain empty on noise")
    true))

(df test-speech-onset-and-speculative-voice-id-success [] -> Bool
  :d "Verifies speech onset buffering, speculative Voice ID extraction, and authorized speaker match"
  (let [(speaker (make-test-speaker "spk-01" 0.80))
        (cfg (vp/make-voice-pipeline-config speaker "browser"))
        (st0 (vp/make-initial-voice-pipeline-state))
        (speech-frame-1 (make-pipeline-test-frame "f-speech-1" -18.0 true))
        (samples-1 (make-constant-samples 32 0.3))
        (res-1 (vp/pipeline-step-frame cfg st0 speech-frame-1 samples-1))
        (st1 (.-next-state res-1))]
    (assert (= (.-action-kind res-1) "collecting-speech") "Initial speech frame must trigger collecting-speech")
    (assert (.-is-utterance-active st1) "Utterance must become active on speech onset")
    (let [(speech-frame-2 (make-pipeline-test-frame "f-speech-2" -15.0 true))
          (samples-2 (make-constant-samples 48 0.3))
          (res-2 (vp/pipeline-step-frame cfg st1 speech-frame-2 samples-2))
          (st2 (.-next-state res-2))]
      (assert (= (.-action-kind res-2) "speculative-verified") "Frame with sufficient samples must trigger speculative verification")
      (assert (.-speaker-matched res-2) "Speaker match must succeed for matching spectral embedding")
      (assert (.-speaker-verified st2) "Pipeline state must record speaker as verified")
      (assert (> (.-speaker-confidence res-2) 0.9) "Confidence score must be high for matching profile")
      true)))

(df test-speculative-voice-id-rejection-and-discard [] -> Bool
  :d "Verifies unauthorized speaker rejection aborts utterance and prevents token leakage"
  (let [(target-emb (list 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
        (speaker (sm/make-speaker-profile "spk-secure" "Admin" target-emb 0.85))
        (cfg (vp/make-voice-pipeline-config speaker "terminal-tui"))
        (st0 (vp/make-initial-voice-pipeline-state))
        (speech-frame-1 (make-pipeline-test-frame "f-speech-1" -16.0 true))
        (samples-1 (make-constant-samples 32 0.3))
        (res-1 (vp/pipeline-step-frame cfg st0 speech-frame-1 samples-1))
        (st1 (.-next-state res-1))
        (speech-frame-2 (make-pipeline-test-frame "f-speech-2" -14.0 true))
        (samples-2 (make-constant-samples 48 0.3))
        (res-2 (vp/pipeline-step-frame cfg st1 speech-frame-2 samples-2))
        (st2 (.-next-state res-2))]
    (assert (= (.-action-kind res-2) "speculative-rejected") "Non-matching speaker must trigger speculative rejection")
    (assert (not (.-speaker-matched res-2)) "Speaker matched must be false")
    (assert (not (.-speaker-verified st2)) "Pipeline state must record speaker as unverified")
    (assert (not (.-is-utterance-active st2)) "Utterance must be aborted immediately upon voice ID rejection")
    (assert (= (list-length (.-utterance-samples st2)) 0) "Utterance buffer must be purged on rejection")
    true))

(df test-conversational-barge-in-interruption [] -> Bool
  :d "Verifies acoustic barge-in interrupts active TTS audio playback instantly"
  (let [(speaker (make-test-speaker "spk-01" 0.80))
        (cfg (vp/make-voice-pipeline-config speaker "browser"))
        (st-init (vp/make-initial-voice-pipeline-state))
        (st-tts (vp/VoicePipelineState
                  :vad-st (.-vad-st st-init)
                  :noise-st (.-noise-st st-init)
                  :preroll (.-preroll st-init)
                  :is-utterance-active false
                  :utterance-frames-count 0
                  :utterance-samples (list)
                  :speaker-checked false
                  :speaker-verified false
                  :speaker-confidence 0.0
                  :frames-total 10
                  :noise-frames-skipped 5
                  :speech-frames-collected 0
                  :active-tts-playing true))
        (speech-frame (make-pipeline-test-frame "f-barge-1" -12.0 true))
        (samples (make-constant-samples 64 0.2))
        (res (vp/pipeline-step-frame cfg st-tts speech-frame samples))
        (st-after (.-next-state res))]
    (assert (= (.-action-kind res) "barge-in") "Voiced speech while TTS playing must trigger barge-in")
    (assert (not (.-active-tts-playing st-after)) "TTS playing flag must be muted immediately")
    (assert (.-is-utterance-active st-after) "Speech utterance collection must commence immediately")
    true))

(df test-local-paraphrasing-and-fast-path [] -> Bool
  :d "Verifies disfluency eradication and command canonicalization"
  (let [(t1 (vp/pipeline-paraphrase-intent "um uh run gate"))
        (t2 (vp/pipeline-paraphrase-intent "ah run tests"))
        (t3 (vp/pipeline-paraphrase-intent "ну типа check status"))
        (t4 (vp/pipeline-paraphrase-intent "э-э запусти гейт"))]
    (assert (= t1 "asl gate") "um uh run gate must canonicalize to asl gate")
    (assert (= t2 "asl test") "ah run tests must canonicalize to asl test")
    (assert (= t3 "asl status") "check status must canonicalize to asl status")
    (assert (= t4 "asl gate") "Cyrillic zapusti gate must canonicalize to asl gate")
    true))

(df test-spoken-tts-formatting [] -> Bool
  :d "Verifies translation of technical execution receipts into speech-friendly natural sentences"
  (let [(s1 (vp/pipeline-format-spoken-tts "(:receipt :exit 0 :asserts 168)"))
        (s2 (vp/pipeline-format-spoken-tts "(:receipt :exit 1 :error-code 404)"))
        (s3 (vp/pipeline-format-spoken-tts "asl gate"))]
    (assert (= s1 "All verification gates passed cleanly with zero errors.") "Exit 0 receipt must format to clean spoken sentence")
    (assert (= s2 "Verification failed with non-zero exit code.") "Failure receipt must format to concise spoken alert")
    (assert (= s3 "Executing full seven-tier verification gate across all packages.") "asl gate must format to spoken notification")
    true))

(df test-terminal-tui-hud-sparklines [] -> Bool
  :d "Verifies Unicode sparkline generation across dynamic dB ranges"
  (let [(h-quiet (vp/render-terminal-sparkline -65.0 false))
        (h-mid (vp/render-terminal-sparkline -35.0 true))
        (h-loud (vp/render-terminal-sparkline -1.0 true))]
    (assert (string-contains? h-quiet "[NOISE]") "Quiet frame must be tagged NOISE")
    (assert (string-contains? h-quiet " ") "Quiet frame must use lowest sparkline glyph")
    (assert (string-contains? h-mid "[VOICE]") "Mid voiced frame must be tagged VOICE")
    (assert (string-contains? h-mid "▄") "Mid voiced frame must use medium sparkline glyph")
    (assert (string-contains? h-loud "█") "Loud frame must use maximum sparkline glyph")
    true))

(df run-tests [] -> Bool
  :d "Executes all unit tests for unified voice pipeline"
  (do
    (assert (test-noise-only-gating-and-skipping) "test-noise-only-gating-and-skipping failed")
    (assert (test-speech-onset-and-speculative-voice-id-success) "test-speech-onset-and-speculative-voice-id-success failed")
    (assert (test-speculative-voice-id-rejection-and-discard) "test-speculative-voice-id-rejection-and-discard failed")
    (assert (test-conversational-barge-in-interruption) "test-conversational-barge-in-interruption failed")
    (assert (test-local-paraphrasing-and-fast-path) "test-local-paraphrasing-and-fast-path failed")
    (assert (test-spoken-tts-formatting) "test-spoken-tts-formatting failed")
    (assert (test-terminal-tui-hud-sparklines) "test-terminal-tui-hud-sparklines failed")
    (println "PASS (asl-voice/voice-pipeline: all 7 test suites passed cleanly with verified assertions)")
    true))
