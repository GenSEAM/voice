(module asl-voice/tests/vad-test
  :d "Dual-case unit tests for Voice Activity Detection, ZCR calculation, and utterance frame slicing"
  :x [run-tests
      test-energy-thresholds
      test-zcr-calculation
      test-vad-hysteresis
      test-slice-utterance-frames]
  :i [(vad :a vad)])

(df test-energy-thresholds [] -> Bool
  :d "Verifies audio frame energy thresholds and boolean speech decision"
  (let [(cfg (vad/make-vad-config 16000))
        (e0 (vad/compute-energy (list)))
        (e-silence (vad/compute-energy (list 0.001 0.002 0.001)))
        (e-speech (vad/compute-energy (list 0.2 0.3 0.25)))]
    (assert (<= e0 0.0001) "Empty samples energy must be 0")
    (assert (not (vad/is-speech-frame? e-silence (.-energy-threshold cfg))) "Silence frame must not trigger is-speech-frame?")
    (assert (vad/is-speech-frame? e-speech (.-energy-threshold cfg)) "Speech frame must trigger is-speech-frame?")
    (assert (not (vad/detect-voice-activity (list 0.001 0.002 0.001) cfg)) "Silence samples must not trigger detect-voice-activity")
    (assert (vad/detect-voice-activity (list 0.2 0.3 0.25) cfg) "Speech samples must trigger detect-voice-activity")
    true))

(df test-zcr-calculation [] -> Bool
  :d "Verifies zero-crossing rate computation for static and alternating signals"
  (let [(z-flat (vad/compute-zcr (list 0.5 0.5 0.5 0.5)))
        (z-cross (vad/compute-zcr (list 0.5 -0.5 0.5 -0.5)))
        (z-empty (vad/compute-zcr (list)))]
    (assert (<= z-flat 0.001) "Flat positive waveform must have 0 zero-crossings")
    (assert (> z-cross 0.99) "Alternating waveform must reach maximum zero-crossing rate")
    (assert (<= z-empty 0.001) "Empty waveform ZCR must be 0")
    true))

(df test-vad-hysteresis [] -> Bool
  :d "Verifies hangover frame hysteresis preserving speech during brief inter-syllable pauses"
  (let [(cfg (vad/VadConfig :sample-rate 16000 :frame-size 160 :energy-threshold 0.02 :zcr-threshold 0.08 :hangover-frames 2))
        (st0 (vad/make-initial-vad-state))
        (st1 (vad/step-vad cfg st0 0.001))
        (st2 (vad/step-vad cfg st1 0.08))
        (st3 (vad/step-vad cfg st2 0.005))
        (st4 (vad/step-vad cfg st3 0.005))
        (st5 (vad/step-vad cfg st4 0.005))]
    (assert (not (.-is-speech st1)) "Silence step on initial state must remain silent")
    (assert (= (.-consecutive-silence st1) 1) "Silence counter must increment to 1")
    (assert (.-is-speech st2) "Speech step must transition is-speech to true")
    (assert (= (.-consecutive-speech st2) 1) "Speech counter must equal 1")
    (assert (.-is-speech st3) "Silence step within hangover 1 must retain speech state")
    (assert (.-is-speech st4) "Silence step within hangover 2 must retain speech state")
    (assert (not (.-is-speech st5)) "Silence step exceeding hangover 2 must transition to false")
    (assert (= (.-consecutive-silence st5) 3) "Consecutive silence must equal 3")
    true))

(df test-slice-utterance-frames [] -> Bool
  :d "Verifies utterance boundary slicing filtering leading and trailing silence"
  (let [(cfg (vad/VadConfig :sample-rate 16000 :frame-size 160 :energy-threshold 0.02 :zcr-threshold 0.08 :hangover-frames 1))
        (f-silence (list 0.001 0.001 0.001))
        (f-speech1 (list 0.2 0.3 0.25))
        (f-speech2 (list 0.15 0.25 0.2))
        (frames (list f-silence f-silence f-speech1 f-speech2 f-silence f-silence f-silence))
        (sliced (vad/slice-utterance-frames frames cfg))
        (empty-sliced (vad/slice-utterance-frames (list) cfg))]
    (assert (= (list-length sliced) 3) "Sliced utterance frames must retain speech frames plus hangover frame")
    (assert (= (list-length empty-sliced) 0) "Empty frames slice must return empty list")
    true))

(df run-tests [] -> Bool
  :d "Executes all VAD unit tests"
  (do
    (assert (test-energy-thresholds) "test-energy-thresholds must pass")
    (assert (test-zcr-calculation) "test-zcr-calculation must pass")
    (assert (test-vad-hysteresis) "test-vad-hysteresis must pass")
    (assert (test-slice-utterance-frames) "test-slice-utterance-frames must pass")
    true))

(run-tests)
