(module asl-voice/vad
  :d "Low-latency Voice Activity Detection (VAD) & Conversational Barge-In Engine in ASL"
  :x [VadConfig VadState BargeInEvent
      make-vad-config make-initial-vad-state
      compute-energy is-speech-frame
      step-vad trigger-barge-in]
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

(df is-speech-frame [(energy Float) (threshold Float)] -> Bool
  :d "Evaluates whether frame energy exceeds speech threshold"
  (> energy threshold))

(df step-vad [(cfg VadConfig) (st VadState) (frame-energy Float)] -> VadState
  :d "Transitions VAD state given new frame energy and hangover logic"
  (let [(speech? (is-speech-frame frame-energy (.-energy-threshold cfg)))]
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

(df trigger-barge-in [(agent-id Str) (timestamp Int64) (latency-ms Float)] -> BargeInEvent
  :d "Constructs immediate barge-in interruption record"
  (BargeInEvent
    :target-agent agent-id
    :timestamp timestamp
    :cutoff-latency-ms latency-ms))
