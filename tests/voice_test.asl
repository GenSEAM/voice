(module asl-voice/test
  :d "Unit tests for voice assistant, VAD, and audio chunk streaming."
  :x [run-tests]
  :i [(vad :a vad)
      (voice :a v)])

(df test-vad-config [] -> Bool
  :d "Verifies VAD config construction."
  (let [(cfg (vad/make-vad-config 16000))]
    (and (= (.-sample-rate cfg) 16000)
         (= (.-frame-size cfg) 160))))

(df test-vad-state [] -> Bool
  :d "Verifies initial VAD idle state."
  (let [(st (vad/make-initial-vad-state))]
    (and (not (.-is-speech st))
         (= (.-consecutive-speech st) 0))))

(df test-audio-chunk [] -> Bool
  :d "Verifies audio chunk processing."
  (let [(chk (v/AudioChunk :id "chk-1" :format (v/pcm-16k) :sample-rate 16000 :byte-length 320 :timestamp 1000))
        (frame (v/process-audio-chunk chk))]
    (and (= (.-chunk-id frame) "chk-1")
         (.-is-final frame))))

(df test-speech-synth [] -> Bool
  :d "Verifies speech synthesis metadata generation."
  (let [(res (v/synthesize-speech-event "Hello world" "eddie-v1"))]
    (> (string-length res) 0)))

(df run-tests [] -> Bool
  :d "Executes all voice test assertions."
  (and (test-vad-config)
       (and (test-vad-state)
            (and (test-audio-chunk)
                 (test-speech-synth)))))
