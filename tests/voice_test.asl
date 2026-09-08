(module asl-voice/test
  :d "Unit tests for voice assistant, VAD, and audio chunk streaming."
  :x [run-tests]
  :i [(vad :a vad)
      (voice :a v)])

(df test-vad-config [] -> Bool
  :d "Verifies VAD config construction."
  (let [(cfg (vad/make-vad-config 16000))]
    (assert (= (.-sample-rate cfg) 16000) "Sample rate must equal 16000")
    (assert (= (.-frame-size cfg) 160) "Frame size must equal 160")
    true))

(df test-vad-state [] -> Bool
  :d "Verifies initial VAD idle state."
  (let [(st (vad/make-initial-vad-state))]
    (assert (not (.-is-speech st)) "Initial VAD state must not be speech")
    (assert (= (.-consecutive-speech st) 0) "Consecutive speech must equal 0")
    true))

(df test-audio-chunk [] -> Bool
  :d "Verifies audio chunk processing."
  (let [(chk (v/AudioChunk :id "chk-1" :format (v/pcm-16k) :sample-rate 16000 :byte-length 320 :timestamp 1000))
        (frame (v/process-audio-chunk chk))]
    (assert (= (.-chunk-id frame) "chk-1") "Chunk id must equal chk-1")
    (assert (.-is-final frame) "Audio frame must be final")
    true))

(df test-speech-synth [] -> Bool
  :d "Verifies speech synthesis metadata generation."
  (let [(res (v/synthesize-speech-event "Hello world" "eddie-v1"))]
    (assert (> (string-length res) 0) "Synthesized speech event must be non-empty")
    true))

(df test-compute-energy [] -> Bool
  :d "Verifies audio frame energy calculation."
  (let [(e0 (vad/compute-energy (list)))
        (e1 (vad/compute-energy (list 0.1 0.2 0.3)))]
    (assert (<= e0 0.001) "Empty samples energy must be 0")
    (assert (> e1 0.0) "Non-empty samples energy must be greater than 0")
    true))

(df run-tests [] -> Bool
  :d "Executes all voice test assertions."
  (do
    (assert (test-vad-config) "test-vad-config must pass")
    (assert (test-vad-state) "test-vad-state must pass")
    (assert (test-compute-energy) "test-compute-energy must pass")
    (assert (test-audio-chunk) "test-audio-chunk must pass")
    (assert (test-speech-synth) "test-speech-synth must pass")
    true))

(run-tests)
