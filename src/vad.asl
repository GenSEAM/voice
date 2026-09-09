(module asl-voice/vad
  :d "Low-latency Voice Activity Detection (VAD) & Conversational Barge-In Engine in ASL"
  :x [VadConfig VadState BargeInEvent
      make-vad-config make-initial-vad-state
      compute-energy compute-zcr
      is-speech-frame is-speech-frame?
      step-vad trigger-barge-in
      detect-voice-activity slice-utterance-frames]
  :i [(core/strings :a s)])

(dfs VadConfig
  (:f sample-rate Int64 "audio sample rate in Hz e.g. 16000")
  (:f frame-size Int64 "number of samples per evaluation frame e.g. 160 or 320")
  (:f energy-threshold Float "energy cutoff below which frame is silence")
  (:f zcr-threshold Float "zero-crossing rate threshold")
  (:f hangover-frames Int64 "number of silence frames before marking speech end"))

(dfs VadState
  (:f is-speech Bool "true if currently within a speech burst")
  (:f consecutive-speech Int64 "count of continuous speech frames")
  (:f consecutive-silence Int64 "count of continuous silence frames")
  (:f current-energy Float "last evaluated frame energy"))

(dfs BargeInEvent
  (:f target-agent Str "agent whose audio output is being interrupted")
  (:f timestamp Int64 "epoch timestamp of interrupt event")
  (:f cutoff-latency-ms Float "measured interrupt detection latency in ms"))

(df make-vad-config [(sample-rate Int64)] -> VadConfig
  :d "Creates standard VAD configuration for voice assistant"
  (VadConfig
    :sample-rate sample-rate
    :frame-size 160
    :energy-threshold 0.015
    :zcr-threshold 0.08
    :hangover-frames 5))

(df make-initial-vad-state [] -> VadState
  :d "Initializes idle VAD state"
  (VadState
    :is-speech false
    :consecutive-speech 0
    :consecutive-silence 0
    :current-energy 0.0))

(df compute-energy [(samples (List Float))] -> Float
  :d "Calculates root-mean-square audio frame energy level"
  (if (list-empty? samples)
      0.0
      (let [(sum (list-fold (fn [(acc Float) (s Float)] (+ acc (* s s))) 0.0 samples))
            (n (list-length samples))]
        (if (> n 0) (/ sum (float n)) 0.0))))

(df is-speech-frame [(energy Float) (threshold Float)] -> Bool
  :d "Evaluates whether frame energy exceeds speech threshold"
  (> energy threshold))

(df is-speech-frame? [(energy Float) (threshold Float)] -> Bool
  :d "Evaluates whether frame energy exceeds speech threshold"
  (> energy threshold))

(df zcr-count-helper [(prev Float) (rest (List Float)) (count Int64)] -> Int64
  :d "Recursive helper counting zero crossings between adjacent samples"
  (if (list-empty? rest)
    count
    (let [(curr (option-or (list-head rest) 0.0))
          (tail (option-or (list-tail rest) (list)))
          (cross? (or (and (< prev 0.0) (> curr 0.0))
                      (and (> prev 0.0) (< curr 0.0))))
          (new-count (if cross? (+ count 1) count))]
      (zcr-count-helper curr tail new-count))))

(df compute-zcr [(samples (List Float))] -> Float
  :d "Calculates zero-crossing rate of PCM audio samples"
  (let [(n (list-length samples))]
    (if (<= n 1)
      0.0
      (let [(h (option-or (list-head samples) 0.0))
            (t (option-or (list-tail samples) (list)))
            (crossings (zcr-count-helper h t 0))]
        (/ (int64-to-float64 crossings) (int64-to-float64 (- n 1)))))))

(df detect-voice-activity [(samples (List Float)) (cfg VadConfig)] -> Bool
  :d "Detects active voice in audio frame by evaluating RMS energy against threshold"
  (if (list-empty? samples)
    false
    (let [(energy (compute-energy samples))]
      (is-speech-frame? energy (.-energy-threshold cfg)))))

(df step-vad [(cfg VadConfig) (st VadState) (frame-energy Float)] -> VadState
  :d "Transitions VAD state given new frame energy and hangover logic"
  (let [(speech? (is-speech-frame? frame-energy (.-energy-threshold cfg)))]
    (if speech?
        (VadState
          :is-speech true
          :consecutive-speech (+ (.-consecutive-speech st) 1)
          :consecutive-silence 0
          :current-energy frame-energy)
        (let [(silence-count (+ (.-consecutive-silence st) 1))
              (still-speaking? (and (.-is-speech st) (<= silence-count (.-hangover-frames cfg))))]
          (VadState
            :is-speech still-speaking?
            :consecutive-speech 0
            :consecutive-silence silence-count
            :current-energy frame-energy)))))

(df slice-frames-helper [(frames (List (List Float))) (cfg VadConfig) (st VadState) (acc (List (List Float)))] -> (List (List Float))
  :d "Recursive helper tracking VAD state across frames and accumulating active utterance frames"
  (if (list-empty? frames)
    acc
    (let [(frame (option-or (list-head frames) (list)))
          (rest (option-or (list-tail frames) (list)))
          (energy (compute-energy frame))
          (next-st (step-vad cfg st energy))
          (next-acc (if (.-is-speech next-st)
                      (list-append acc (list frame))
                      acc))]
      (slice-frames-helper rest cfg next-st next-acc))))

(df slice-utterance-frames [(frames (List (List Float))) (cfg VadConfig)] -> (List (List Float))
  :d "Slices contiguous utterance audio frames using VAD state machine and hangover frames"
  (slice-frames-helper frames cfg (make-initial-vad-state) (list)))

(df trigger-barge-in [(agent-id Str) (timestamp Int64) (latency-ms Float)] -> BargeInEvent
  :d "Constructs immediate barge-in interruption record"
  (BargeInEvent
    :target-agent agent-id
    :timestamp timestamp
    :cutoff-latency-ms latency-ms))
