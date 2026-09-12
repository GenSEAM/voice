(module asl-voice/vad
  :d "Low-latency Voice Activity Detection (VAD) & Conversational Barge-In Engine in ASL"
  :x [VadConfig VadState BargeInEvent
      make-vad-config make-initial-vad-state
      compute-energy compute-zcr
      is-speech-frame is-speech-frame?
      step-vad trigger-barge-in
      detect-voice-activity slice-utterance-frames]
  :i [(asl-text/string :a s)])

(dfs VadConfig
  (:f sample-rate I64 "audio sample rate in Hz e.g. 16000")
  (:f frame-size I64 "number of samples per evaluation frame e.g. 160 or 320")
  (:f energy-threshold F64 "energy cutoff below which frame is silence")
  (:f zcr-threshold F64 "zero-crossing rate threshold")
  (:f hangover-frames I64 "number of silence frames before marking speech end"))

(dfs VadState
  (:f is-speech Bool "true if currently within a speech burst")
  (:f consecutive-speech I64 "count of continuous speech frames")
  (:f consecutive-silence I64 "count of continuous silence frames")
  (:f current-energy F64 "last evaluated frame energy"))

(dfs BargeInEvent
  (:f target-agent Str "agent whose audio output is being interrupted")
  (:f timestamp I64 "epoch timestamp of interrupt event")
  (:f cutoff-latency-ms F64 "measured interrupt detection latency in ms"))

(df make-vad-config [(sample-rate I64)] -> VadConfig
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

(df compute-energy [(samples (List F64))] -> F64
  :d "Calculates root-mean-square audio frame energy level"
  (if (list-empty? samples)
      0.0
      (let [(sum (list-fold (fn [(acc F64) (s F64)] (+ acc (* s s))) 0.0 samples))
            (n (list-length samples))]
        (if (> n 0) (/ sum (as-f64 n)) 0.0))))

(df is-speech-frame [(energy F64) (threshold F64)] -> Bool
  :d "Evaluates whether frame energy exceeds speech threshold"
  (> energy threshold))

(df is-speech-frame? [(energy F64) (threshold F64)] -> Bool
  :d "Evaluates whether frame energy exceeds speech threshold"
  (> energy threshold))

(df zcr-count-helper [(prev F64) (rest (List F64)) (count I64)] -> I64
  :d "Recursive helper counting zero crossings between adjacent samples"
  (if (list-empty? rest)
    count
    (let [(curr (option-or (list-head rest) 0.0))
          (tail (option-or (list-tail rest) (list)))
          (cross? (or (and (< prev 0.0) (> curr 0.0))
                      (and (> prev 0.0) (< curr 0.0))))
          (new-count (if cross? (+ count 1) count))]
      (zcr-count-helper curr tail new-count))))

(df compute-zcr [(samples (List F64))] -> F64
  :d "Calculates zero-crossing rate of PCM audio samples"
  (let [(n (list-length samples))]
    (if (<= n 1)
      0.0
      (let [(h (option-or (list-head samples) 0.0))
            (t (option-or (list-tail samples) (list)))
            (crossings (zcr-count-helper h t 0))]
        (/ (int64-to-float64 crossings) (int64-to-float64 (- n 1)))))))

(df detect-voice-activity [(samples (List F64)) (cfg VadConfig)] -> Bool
  :d "Detects active voice in audio frame by evaluating RMS energy against threshold"
  (if (list-empty? samples)
    false
    (let [(energy (compute-energy samples))]
      (is-speech-frame? energy (.-energy-threshold cfg)))))

(df step-vad [(cfg VadConfig) (st VadState) (frame-energy F64)] -> VadState
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

(df slice-frames-helper [(frames (List (List F64))) (cfg VadConfig) (st VadState) (acc (List (List F64)))] -> (List (List F64))
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

(df slice-utterance-frames [(frames (List (List F64))) (cfg VadConfig)] -> (List (List F64))
  :d "Slices contiguous utterance audio frames using VAD state machine and hangover frames"
  (slice-frames-helper frames cfg (make-initial-vad-state) (list)))

(df trigger-barge-in [(agent-id Str) (timestamp I64) (latency-ms F64)] -> BargeInEvent
  :d "Constructs immediate barge-in interruption record"
  (BargeInEvent
    :target-agent agent-id
    :timestamp timestamp
    :cutoff-latency-ms latency-ms))
