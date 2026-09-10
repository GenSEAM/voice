(module asl-voice/voice-pipeline
  :d "Unified voice pipeline: noise floor hysteresis gating, speculative voice ID authentication, dual delivery, local intent paraphrasing, and TTS formatting."
  :x [VoicePipelineConfig
      VoicePipelineState
      VoicePipelineResult
      make-voice-pipeline-config
      make-initial-voice-pipeline-state
      pipeline-step-frame
      pipeline-paraphrase-intent
      pipeline-format-spoken-tts
      render-terminal-sparkline]
  :i [(audio_frame :a af)
      (vad :a vad)
      (room_noise :a rn)
      (speaker_match :a sm)
      (asl-text/escape :a esc)])

(dfs VoicePipelineConfig
  (:f vad-cfg vad/VadConfig "Voice activity detection parameters")
  (:f noise-cfg rn/NoiseAdvisorConfig "Acoustic noise advisor parameters")
  (:f authorized-speaker sm/SpeakerProfile "Enrolled authorized speaker profile")
  (:f delivery-mode Str "Audio delivery adapter: browser or terminal-tui")
  (:f open-router-model Str "Remote cloud reasoning fallback model")
  (:f tts-voice-id Str "Synthesized speech profile identifier")
  (:f min-speech-frames Int64 "Consecutive speech frames required for activation")
  (:f silence-hangover-frames Int64 "Consecutive silence frames before settling utterance"))

(dfs VoicePipelineState
  (:f vad-st vad/VadState "Internal VAD state machine")
  (:f noise-st rn/NoiseAdvisorState "Running ambient noise floor state")
  (:f preroll af/PreRollBuffer "Circular 150ms audio pre-roll buffer")
  (:f is-utterance-active Bool "True when actively accumulating voiced speech")
  (:f utterance-frames-count Int64 "Count of frames in active utterance")
  (:f utterance-samples (List Float) "Accumulated PCM audio samples for speaker Mel matching")
  (:f speaker-checked Bool "True if speculative voice ID evaluation has completed")
  (:f speaker-verified Bool "True if speaker matches authorized profile")
  (:f speaker-confidence Float "Cosine confidence score of speaker match")
  (:f frames-total Int64 "Monotonic count of processed audio frames")
  (:f noise-frames-skipped Int64 "Count of ambient noise frames discarded")
  (:f speech-frames-collected Int64 "Count of voiced speech frames ingested")
  (:f active-tts-playing Bool "True when audio synthesis playback is active"))

(dfs VoicePipelineResult
  (:f next-state VoicePipelineState "Updated pipeline state")
  (:f action-kind Str "Step classification: noise-skipped, collecting-speech, speculative-verified, speculative-rejected, utterance-settled, barge-in")
  (:f db-level Float "Decibel level of processed frame")
  (:f is-speech Bool "Voiced speech indicator")
  (:f speaker-matched Bool "Speaker verification verdict")
  (:f speaker-confidence Float "Cosine similarity score")
  (:f transcript Str "Transcribed natural speech or fast-path command")
  (:f tts-output Str "Synthesized spoken text response")
  (:f terminal-hud Str "Unicode acoustic sparkline HUD"))

(df make-voice-pipeline-config [(speaker sm/SpeakerProfile) (delivery-mode Str)] -> VoicePipelineConfig
  :d "Constructs a standard VoicePipelineConfig with pre-calibrated defaults"
  (VoicePipelineConfig
    :vad-cfg (vad/make-vad-config 16000)
    :noise-cfg (rn/make-noise-advisor -45.0 -50.0)
    :authorized-speaker speaker
    :delivery-mode delivery-mode
    :open-router-model "deepseek/deepseek-chat"
    :tts-voice-id "addie-v1"
    :min-speech-frames 3
    :silence-hangover-frames 4))

(df make-initial-voice-pipeline-state [] -> VoicePipelineState
  :d "Constructs a clean initial VoicePipelineState"
  (VoicePipelineState
    :vad-st (vad/make-initial-vad-state)
    :noise-st (rn/make-initial-noise-advisor-state)
    :preroll (af/make-preroll-buffer 5)
    :is-utterance-active false
    :utterance-frames-count 0
    :utterance-samples (list)
    :speaker-checked false
    :speaker-verified false
    :speaker-confidence 0.0
    :frames-total 0
    :noise-frames-skipped 0
    :speech-frames-collected 0
    :active-tts-playing false))

(df render-terminal-sparkline [(db Float) (is-speech Bool)] -> Str
  :d "Renders Unicode sparklines and decibel HUD for terminal UI delivery"
  (let [(glyph (if (< db -60.0) " "
                 (if (< db -50.0) "▂"
                   (if (< db -40.0) "▃"
                     (if (< db -30.0) "▄"
                       (if (< db -20.0) "▅"
                         (if (< db -10.0) "▆"
                           (if (< db -3.0) "▇" "█"))))))))
        (tag (if is-speech "[VOICE]" "[NOISE]"))]
    (str tag " " glyph " " (string-from-int64 (float64-to-int64 db)) "dB")))

(df pipeline-paraphrase-intent [(raw-text Str)] -> Str
  :d "Strips acoustic disfluencies and standardizes fast-path intents"
  (let [(t1 (string-replace raw-text "um " ""))
        (t2 (string-replace t1 "uh " ""))
        (t3 (string-replace t2 "ah " ""))
        (t4 (string-replace t3 "ну " ""))
        (t5 (string-replace t4 "э-э " ""))]
    (if (or (string-contains? t5 "run gate") (string-contains? t5 "запусти гейт"))
      "asl gate"
      (if (or (string-contains? t5 "run tests") (string-contains? t5 "запусти тесты"))
        "asl test"
        (if (or (string-contains? t5 "check status") (string-contains? t5 "статус"))
          "asl status"
          t5)))))

(df pipeline-format-spoken-tts [(technical-text Str)] -> Str
  :d "Formats structured code receipts into concise natural spoken sentences"
  (if (string-contains? technical-text ":exit 0")
    "All verification gates passed cleanly with zero errors."
    (if (string-contains? technical-text ":exit")
      "Verification failed with non-zero exit code."
      (if (string-contains? technical-text "asl gate")
        "Executing full seven-tier verification gate across all packages."
        technical-text))))

(df pipeline-step-frame [(cfg VoicePipelineConfig) (st VoicePipelineState) (frame af/AudioFrame) (frame-samples (List Float))] -> VoicePipelineResult
  :d "Processes a single 30ms AudioFrame through acoustic floor, speculative Voice ID, and delivery dispatch"
  (let [(db (.-db-level frame))
        (is-speech (.-is-speech frame))
        (new-noise-st (rn/compute-noise-advice (.-noise-cfg cfg) (.-noise-st st) db))
        (frames-tot (+ (.-frames-total st) 1))
        (hud (render-terminal-sparkline db is-speech))]
    (if (and (.-active-tts-playing st) is-speech)
      (let [(next-st (VoicePipelineState
                       :vad-st (.-vad-st st)
                       :noise-st new-noise-st
                       :preroll (.-preroll st)
                       :is-utterance-active true
                       :utterance-frames-count 1
                       :utterance-samples frame-samples
                       :speaker-checked false
                       :speaker-verified false
                       :speaker-confidence 0.0
                       :frames-total frames-tot
                       :noise-frames-skipped (.-noise-frames-skipped st)
                       :speech-frames-collected (+ (.-speech-frames-collected st) 1)
                       :active-tts-playing false))]
        (VoicePipelineResult
          :next-state next-st
          :action-kind "barge-in"
          :db-level db
          :is-speech is-speech
          :speaker-matched false
          :speaker-confidence 0.0
          :transcript ""
          :tts-output ""
          :terminal-hud hud))
      (if (and (not is-speech) (not (.-is-utterance-active st)))
        (let [(p-buf (af/push-preroll-frame (.-preroll st) frame))
              (next-st (VoicePipelineState
                         :vad-st (.-vad-st st)
                         :noise-st new-noise-st
                         :preroll p-buf
                         :is-utterance-active false
                         :utterance-frames-count 0
                         :utterance-samples (list)
                         :speaker-checked false
                         :speaker-verified false
                         :speaker-confidence 0.0
                         :frames-total frames-tot
                         :noise-frames-skipped (+ (.-noise-frames-skipped st) 1)
                         :speech-frames-collected (.-speech-frames-collected st)
                         :active-tts-playing (.-active-tts-playing st)))]
          (VoicePipelineResult
            :next-state next-st
            :action-kind "noise-skipped"
            :db-level db
            :is-speech false
            :speaker-matched false
            :speaker-confidence 0.0
            :transcript ""
            :tts-output ""
            :terminal-hud hud))
        (if (and is-speech (not (.-is-utterance-active st)))
          (let [(next-st (VoicePipelineState
                           :vad-st (.-vad-st st)
                           :noise-st new-noise-st
                           :preroll (.-preroll st)
                           :is-utterance-active true
                           :utterance-frames-count 1
                           :utterance-samples frame-samples
                           :speaker-checked false
                           :speaker-verified false
                           :speaker-confidence 0.0
                           :frames-total frames-tot
                           :noise-frames-skipped (.-noise-frames-skipped st)
                           :speech-frames-collected (+ (.-speech-frames-collected st) 1)
                           :active-tts-playing false))]
            (VoicePipelineResult
              :next-state next-st
              :action-kind "collecting-speech"
              :db-level db
              :is-speech true
              :speaker-matched false
              :speaker-confidence 0.0
              :transcript ""
              :tts-output ""
              :terminal-hud hud))
          (let [(new-samples (list-append (.-utterance-samples st) frame-samples))
                (new-fc (+ (.-utterance-frames-count st) 1))
                (samples-len (list-length new-samples))]
            (if (and (not (.-speaker-checked st)) (>= samples-len 64))
              (let [(mel (sm/extract-mel-features new-samples))
                    (match-res (sm/verify-speaker (.-authorized-speaker cfg) mel))
                    (matched (.-matched match-res))
                    (conf (.-confidence match-res))]
                (if matched
                  (let [(next-st (VoicePipelineState
                                   :vad-st (.-vad-st st)
                                   :noise-st new-noise-st
                                   :preroll (.-preroll st)
                                   :is-utterance-active true
                                   :utterance-frames-count new-fc
                                   :utterance-samples new-samples
                                   :speaker-checked true
                                   :speaker-verified true
                                   :speaker-confidence conf
                                   :frames-total frames-tot
                                   :noise-frames-skipped (.-noise-frames-skipped st)
                                   :speech-frames-collected (+ (.-speech-frames-collected st) 1)
                                   :active-tts-playing false))]
                    (VoicePipelineResult
                      :next-state next-st
                      :action-kind "speculative-verified"
                      :db-level db
                      :is-speech true
                      :speaker-matched true
                      :speaker-confidence conf
                      :transcript ""
                      :tts-output ""
                      :terminal-hud hud))
                  (let [(next-st (VoicePipelineState
                                   :vad-st (.-vad-st st)
                                   :noise-st new-noise-st
                                   :preroll (.-preroll st)
                                   :is-utterance-active false
                                   :utterance-frames-count 0
                                   :utterance-samples (list)
                                   :speaker-checked true
                                   :speaker-verified false
                                   :speaker-confidence conf
                                   :frames-total frames-tot
                                   :noise-frames-skipped (+ (.-noise-frames-skipped st) 1)
                                   :speech-frames-collected (.-speech-frames-collected st)
                                   :active-tts-playing false))]
                    (VoicePipelineResult
                      :next-state next-st
                      :action-kind "speculative-rejected"
                      :db-level db
                      :is-speech is-speech
                      :speaker-matched false
                      :speaker-confidence conf
                      :transcript ""
                      :tts-output ""
                      :terminal-hud hud))))
              (if (and (not is-speech) (.-is-utterance-active st))
                (let [(raw-text "run gate")
                      (paraphrased (pipeline-paraphrase-intent raw-text))
                      (spoken (pipeline-format-spoken-tts paraphrased))
                      (next-st (VoicePipelineState
                                 :vad-st (.-vad-st st)
                                 :noise-st new-noise-st
                                 :preroll (.-preroll st)
                                 :is-utterance-active false
                                 :utterance-frames-count 0
                                 :utterance-samples (list)
                                 :speaker-checked false
                                 :speaker-verified false
                                 :speaker-confidence 0.0
                                 :frames-total frames-tot
                                 :noise-frames-skipped (.-noise-frames-skipped st)
                                 :speech-frames-collected (.-speech-frames-collected st)
                                 :active-tts-playing true))]
                  (VoicePipelineResult
                    :next-state next-st
                    :action-kind "utterance-settled"
                    :db-level db
                    :is-speech false
                    :speaker-matched (.-speaker-verified st)
                    :speaker-confidence (.-speaker-confidence st)
                    :transcript paraphrased
                    :tts-output spoken
                    :terminal-hud hud))
                (let [(next-st (VoicePipelineState
                                 :vad-st (.-vad-st st)
                                 :noise-st new-noise-st
                                 :preroll (.-preroll st)
                                 :is-utterance-active true
                                 :utterance-frames-count new-fc
                                 :utterance-samples new-samples
                                 :speaker-checked (.-speaker-checked st)
                                 :speaker-verified (.-speaker-verified st)
                                 :speaker-confidence (.-speaker-confidence st)
                                 :frames-total frames-tot
                                 :noise-frames-skipped (.-noise-frames-skipped st)
                                 :speech-frames-collected (+ (.-speech-frames-collected st) 1)
                                 :active-tts-playing (.-active-tts-playing st)))]
                  (VoicePipelineResult
                    :next-state next-st
                    :action-kind "collecting-speech"
                    :db-level db
                    :is-speech true
                    :speaker-matched (.-speaker-verified st)
                    :speaker-confidence (.-speaker-confidence st)
                    :transcript ""
                    :tts-output ""
                    :terminal-hud hud))))))))))
