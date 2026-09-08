(module asl-voice/tests/voice-intel-test
  :d "Unit verification suite for audio frames, pre-roll buffer, room noise hysteresis, speaker matching, and Russian phonetics"
  :x [run-tests
      test-audio-frame-chunking
      test-preroll-ring-buffer
      test-noise-floor-tracking
      test-room-noise-hysteresis
      test-speaker-verification
      test-cyrillic-phonetics-and-levenshtein]
  :i [(audio_frame :a af)
      (room_noise :a rn)
      (speaker_match :a sm)
      (heard_name :a hn)])

(df make-samples [(n Int64) (val Float)] -> (List Float)
  :d "Generates a list of n identical float samples"
  (if (<= n 0)
    (list)
    (list-cons val (make-samples (- n 1) val))))

(df make-dummy-frame [(idx Int64)] -> af/AudioFrame
  :d "Constructs a test AudioFrame with distinct frame ID"
  (af/AudioFrame
    :frame-id (string-append "f-" (string-from-int64 idx))
    :samples-count 480
    :duration-ms 30
    :rms-energy 0.05
    :db-level -26.0
    :is-speech false
    :timestamp-ms (* idx 30)))

(df test-audio-frame-chunking [] -> Bool
  :d "Verifies 30ms chunking, RMS energy, silence clamping, and speech threshold comparison"
  (let [(samples-480 (make-samples 480 0.5))
        (frame (af/make-audio-frame "frame-chunk-1" samples-480 -45.0 1000))
        (silence-frame (af/make-audio-frame "silence-1" (list) -45.0 1030))]
    (do
      (assert (= (.-samples-count frame) 480) "frame sample count must equal 480 at 16kHz")
      (assert (= (.-duration-ms frame) 30) "frame duration must equal 30ms")
      (assert (and (> (.-rms-energy frame) 0.49) (< (.-rms-energy frame) 0.51)) "frame energy for 0.5 amplitude must equal 0.5")
      (assert (= (.-db-level silence-frame) -70.0) "digital silence must clamp at minimum -70.0 dB")
      (assert (.-is-speech frame) "frame at -6dB must trigger speech flag against -45dB threshold")
      true)))

(df test-preroll-ring-buffer [] -> Bool
  :d "Verifies 150ms pre-roll ring buffer capacity, FIFO eviction, and chronological drain"
  (let [(buf0 (af/make-preroll-buffer 5))
        (f1 (make-dummy-frame 1))
        (f2 (make-dummy-frame 2))
        (f3 (make-dummy-frame 3))
        (f4 (make-dummy-frame 4))
        (f5 (make-dummy-frame 5))
        (f6 (make-dummy-frame 6))
        (buf1 (af/push-preroll-frame buf0 f1))
        (buf2 (af/push-preroll-frame buf1 f2))
        (buf3 (af/push-preroll-frame buf2 f3))
        (buf4 (af/push-preroll-frame buf3 f4))
        (buf5 (af/push-preroll-frame buf4 f5))
        (buf6 (af/push-preroll-frame buf5 f6))
        (drained (af/drain-preroll-frames buf6))
        (head-frame (option-or (list-head drained) f1))]
    (do
      (assert (and (= (.-count buf0) 0) (= (.-capacity buf0) 5)) "initial buffer count must be 0 and capacity 5")
      (assert (= (.-count buf3) 3) "buffer count after 3 pushes must equal 3")
      (assert (= (.-frame-id head-frame) "f-2") "ring buffer wrap-around must evict oldest frame f-1")
      (assert (= (.-count buf6) 5) "buffer count after 6 pushes must remain clamped at capacity 5")
      (assert (= (list-length drained) 5) "drained frames list length must equal 5 in chronological FIFO order")
      true)))

(df test-noise-floor-tracking [] -> Bool
  :d "Verifies dynamic noise floor adaptation, speech freezing, and boundary clamping"
  (let [(f0 -55.0)
        (f-adapt (af/track-noise-floor f0 -65.0 false))
        (f-frozen (af/track-noise-floor -55.0 -30.0 true))
        (f-min (af/track-noise-floor -70.0 -90.0 false))
        (f-max (af/track-noise-floor -25.0 0.0 false))
        (f-burst1 (af/track-noise-floor -50.0 -20.0 true))
        (f-burst2 (af/track-noise-floor f-burst1 -15.0 true))]
    (do
      (assert (< f-adapt f0) "running noise floor must adapt downwards towards quiet background")
      (assert (= f-frozen -55.0) "running noise floor must freeze when is-speech is true")
      (assert (= f-min -70.0) "noise floor must clamp at minimum -70.0 dB")
      (assert (= f-max -25.0) "noise floor must clamp at maximum -25.0 dB")
      (assert (= f-burst2 -50.0) "noise floor must remain unchanged across repeated speech bursts")
      true)))

(df test-room-noise-hysteresis [] -> Bool
  :d "Verifies dual-threshold hysteresis state transitions, anti-flapping, and dead mic detection"
  (let [(cfg (rn/make-noise-advisor -45.0 -50.0))
        (st0 (rn/make-initial-noise-advisor-state))
        (s1 (rn/compute-noise-advice cfg st0 -40.0))
        (s2 (rn/compute-noise-advice cfg s1 -40.0))
        (s3 (rn/compute-noise-advice cfg s2 -40.0))
        (s4 (rn/compute-noise-advice cfg s3 -40.0))
        (s5 (rn/compute-noise-advice cfg s4 -40.0))
        (s-mid (rn/compute-noise-advice cfg s5 -47.0))
        (c1 (rn/compute-noise-advice cfg s-mid -55.0))
        (c2 (rn/compute-noise-advice cfg c1 -55.0))
        (c3 (rn/compute-noise-advice cfg c2 -55.0))
        (c4 (rn/compute-noise-advice cfg c3 -55.0))
        (c5 (rn/compute-noise-advice cfg c4 -55.0))
        (d1 (rn/compute-noise-advice cfg st0 -70.0))
        (d2 (rn/compute-noise-advice cfg d1 -70.0))
        (d3 (rn/compute-noise-advice cfg d2 -70.0))
        (d4 (rn/compute-noise-advice cfg d3 -70.0))
        (d5 (rn/compute-noise-advice cfg d4 -70.0))]
    (do
      (assert (and (= (.-advice-code st0) "OK") (= (.-persistence-count st0) 0)) "initial room advisor state must be OK with 0 persistence")
      (assert (and (not (.-is-noisy s1)) (and (= (.-advice-code s1) "OK") (and (.-is-noisy s5) (= (.-advice-code s5) "ROOM_TOO_NOISY")))) "4 frames at -40dB must remain OK, 5th frame must transition to ROOM_TOO_NOISY")
      (assert (and (.-is-noisy s-mid) (= (.-advice-code s-mid) "ROOM_TOO_NOISY")) "intermediate level -47dB must retain ROOM_TOO_NOISY without flapping")
      (assert (and (.-is-noisy c1) (and (not (.-is-noisy c5)) (= (.-advice-code c5) "OK"))) "clearing back to OK must require 5 consecutive quiet frames below -50dB")
      (assert (= (.-advice-code d5) "MIC_MUTED_OR_DEAD") "sustained -70dB silence must trigger MIC_MUTED_OR_DEAD")
      true)))

(df test-speaker-verification [] -> Bool
  :d "Verifies cosine similarity, threshold matching, non-match rejection, and cosine distance"
  (let [(v1 (list 1.0 0.0 0.0))
        (v2 (list 0.0 1.0 0.0))
        (sim-id (sm/cosine-similarity v1 v1))
        (sim-ortho (sm/cosine-similarity v1 v2))
        (prof (sm/make-speaker-profile "spk-1" "Alice" v1 0.75))
        (res-match (sm/verify-speaker prof v1))
        (res-no-match (sm/verify-speaker prof v2))
        (expected-dist (- 1.0 (.-confidence res-no-match)))]
    (do
      (assert (= sim-id 1.0) "identical vectors must yield cosine similarity 1.0")
      (assert (= sim-ortho 0.0) "orthogonal vectors must yield cosine similarity 0.0")
      (assert (.-matched res-match) "identical embedding verification must match candidate profile")
      (assert (not (.-matched res-no-match)) "orthogonal embedding verification must reject match")
      (assert (= (.-distance res-no-match) expected-dist) "cosine distance must equal 1.0 minus confidence")
      true)))

(df test-cyrillic-phonetics-and-levenshtein [] -> Bool
  :d "Verifies Russian digraph transliteration, exact Levenshtein distance, and candidate matching"
  (let [(t1 (hn/transliterate-cyrillic "шроуди"))
        (t2 (hn/transliterate-cyrillic "щека"))
        (dist1 (hn/levenshtein-distance "shrodi" "shrody"))
        (dist2 (hn/levenshtein-distance "shroudi" "shrody"))
        (t-upper (hn/transliterate-cyrillic "ШРОУДИ"))
        (cands (list "asex" "shrody" "core"))
        (m1 (hn/match-heard-name "шроуди" cands 2))
        (m2 (hn/match-heard-name "шроуди" (list "completely-different") 1))]
    (do
      (assert (and (= t1 "shroudi") (= t2 "scheka")) "longest digraph transliteration must map correctly")
      (assert (and (= dist1 1) (= dist2 2)) "levenshtein distance must accurately measure edit distance")
      (assert (= t-upper "shroudi") "transliteration must normalize uppercase Cyrillic input")
      (assert (and (.-is-match m1) (= (.-matched-target m1) "shrody")) "phonetic heard name must select closest project candidate")
      (assert (not (.-is-match m2)) "candidate matching must reject targets exceeding max edit distance")
      true)))

(df run-tests [] -> Bool
  :d "Executes all 6 voice intelligence test suites"
  (do
    (test-audio-frame-chunking)
    (test-preroll-ring-buffer)
    (test-noise-floor-tracking)
    (test-room-noise-hysteresis)
    (test-speaker-verification)
    (test-cyrillic-phonetics-and-levenshtein)
    true))
