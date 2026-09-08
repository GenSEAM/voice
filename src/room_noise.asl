(module asl-voice/room-noise
  :d "Dual dynamic noise floor tracking (-70dB to -25dB) with hysteresis advice"
  :x [NoiseAdvisorConfig
      NoiseAdvisorState
      make-noise-advisor
      make-initial-noise-advisor-state
      compute-noise-advice]
  :i [])

(dfs NoiseAdvisorConfig
  (:f noisy-threshold-db Float "noisy alert trigger threshold in dB e.g. -45.0")
  (:f quiet-threshold-db Float "noisy alert clear threshold in dB e.g. -50.0")
  (:f min-floor-db Float "minimum clamped floor bound in dB e.g. -70.0")
  (:f max-floor-db Float "maximum clamped floor bound in dB e.g. -25.0")
  (:f dwell-frames Int64 "minimum consecutive frames required to transition state"))

(dfs NoiseAdvisorState
  (:f current-floor-db Float "current estimated ambient noise floor in dB")
  (:f is-noisy Bool "true if currently in noisy room alert state")
  (:f persistence-count Int64 "count of consecutive frames matching candidate transition")
  (:f advice-code Str "categorical advice code OK, ROOM_TOO_NOISY, MIC_MUTED_OR_DEAD")
  (:f advice-message Str "human readable acoustic explanation"))

(df clamp-floor [(val Float) (min-db Float) (max-db Float)] -> Float
  :d "Clamps noise floor between min and max bounds"
  (if (< val min-db)
    min-db
    (if (> val max-db)
      max-db
      val)))

(df update-running-floor [(curr Float) (frame-db Float) (min-db Float) (max-db Float)] -> Float
  :d "Smooths noise floor estimate using exponential moving average clamped to range"
  (let [(raw (+ (* curr 0.95) (* frame-db 0.05)))]
    (clamp-floor raw min-db max-db)))

(df make-noise-advisor [(noisy-db Float) (quiet-db Float)] -> NoiseAdvisorConfig
  :d "Instantiates noise advisor configuration with dual hysteresis thresholds"
  (NoiseAdvisorConfig
    :noisy-threshold-db noisy-db
    :quiet-threshold-db quiet-db
    :min-floor-db -70.0
    :max-floor-db -25.0
    :dwell-frames 5))

(df make-initial-noise-advisor-state [] -> NoiseAdvisorState
  :d "Constructs initial quiet and healthy advisor state"
  (NoiseAdvisorState
    :current-floor-db -55.0
    :is-noisy false
    :persistence-count 0
    :advice-code "OK"
    :advice-message "Room acoustic environment is optimal"))

(df compute-noise-advice [(cfg NoiseAdvisorConfig) (st NoiseAdvisorState) (frame-db Float)] -> NoiseAdvisorState
  :d "Transitions advisor state given current frame level using dual-threshold hysteresis"
  (let [(min-floor (.-min-floor-db cfg))
        (max-floor (.-max-floor-db cfg))
        (dwell (.-dwell-frames cfg))
        (is-silent (<= frame-db min-floor))
        (new-floor (if is-silent
                     min-floor
                     (update-running-floor (.-current-floor-db st) frame-db min-floor max-floor)))]
    (if is-silent
      (let [(new-count (+ (.-persistence-count st) 1))]
        (if (>= new-count dwell)
          (NoiseAdvisorState
            :current-floor-db min-floor
            :is-noisy false
            :persistence-count new-count
            :advice-code "MIC_MUTED_OR_DEAD"
            :advice-message "Microphone input is completely silent or disconnected")
          (NoiseAdvisorState
            :current-floor-db new-floor
            :is-noisy false
            :persistence-count new-count
            :advice-code (.-advice-code st)
            :advice-message (.-advice-message st))))
      (if (.-is-noisy st)
        (if (< frame-db (.-quiet-threshold-db cfg))
          (let [(new-count (+ (.-persistence-count st) 1))]
            (if (>= new-count dwell)
              (NoiseAdvisorState
                :current-floor-db new-floor
                :is-noisy false
                :persistence-count 0
                :advice-code "OK"
                :advice-message "Room acoustic environment is optimal")
              (NoiseAdvisorState
                :current-floor-db new-floor
                :is-noisy true
                :persistence-count new-count
                :advice-code "ROOM_TOO_NOISY"
                :advice-message "Ambient room noise exceeds recommended acoustic threshold")))
          (NoiseAdvisorState
            :current-floor-db new-floor
            :is-noisy true
            :persistence-count 0
            :advice-code "ROOM_TOO_NOISY"
            :advice-message "Ambient room noise exceeds recommended acoustic threshold"))
        (if (>= frame-db (.-noisy-threshold-db cfg))
          (let [(new-count (+ (.-persistence-count st) 1))]
            (if (>= new-count dwell)
              (NoiseAdvisorState
                :current-floor-db new-floor
                :is-noisy true
                :persistence-count 0
                :advice-code "ROOM_TOO_NOISY"
                :advice-message "Ambient room noise exceeds recommended acoustic threshold")
              (NoiseAdvisorState
                :current-floor-db new-floor
                :is-noisy false
                :persistence-count new-count
                :advice-code "OK"
                :advice-message "Room acoustic environment is optimal")))
          (NoiseAdvisorState
            :current-floor-db new-floor
            :is-noisy false
            :persistence-count 0
            :advice-code "OK"
            :advice-message "Room acoustic environment is optimal"))))))
