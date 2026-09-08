(module asl-voice/audio-frame
  :d "Frame-based audio physics, 150ms pre-roll ring buffer, and running noise floor tracking"
  :x [AudioFrame PreRollBuffer make-audio-frame compute-frame-energy compute-db-level make-preroll-buffer push-preroll-frame drain-preroll-frames track-noise-floor]
  :i [])

(dfs AudioFrame
  (:f frame-id Str "Unique identifier for audio frame")
  (:f samples-count Int64 "Number of samples in frame")
  (:f duration-ms Int64 "Frame duration in milliseconds")
  (:f rms-energy Float "Root-mean-square energy level")
  (:f db-level Float "Clamped decibel level relative to full scale")
  (:f is-speech Bool "Boolean flag indicating speech activity")
  (:f timestamp-ms Int64 "Monotonic millisecond timestamp"))

(dfs PreRollBuffer
  (:f capacity Int64 "Maximum buffer frame capacity")
  (:f frames (List AudioFrame) "Ordered list of retained audio frames")
  (:f count Int64 "Current number of stored frames")
  (:f head Int64 "Circular write index"))

(df sqrt-fast [(x Float)] -> Float
  :d "Newton-Raphson square root approximation"
  (if (<= x 0.0)
      0.0
      (let [(g0 (/ x 2.0))
            (g1 (/ (+ g0 (/ x g0)) 2.0))
            (g2 (/ (+ g1 (/ x g1)) 2.0))
            (g3 (/ (+ g2 (/ x g2)) 2.0))
            (g4 (/ (+ g3 (/ x g3)) 2.0))]
        (/ (+ g4 (/ x g4)) 2.0))))

(df sum-sq-helper [(samples (List Float)) (acc Float)] -> Float
  :d "Helper computing sum of squared PCM samples"
  (if (list-empty? samples)
      acc
      (let [(h (option-or (list-head samples) 0.0))
            (t (option-or (list-tail samples) (list)))]
        (sum-sq-helper t (+ acc (* h h))))))

(df compute-frame-energy [(samples (List Float))] -> Float
  :d "Computes root-mean-square energy over PCM sample list"
  (let [(n (list-length samples))]
    (if (<= n 0)
        0.0
        (let [(sum-sq (sum-sq-helper samples 0.0))
              (mean-sq (/ sum-sq (int64-to-float64 n)))]
          (sqrt-fast mean-sq)))))

(df db-norm [(x Float)] -> Float
  :d "Evaluates log10 polynomial on normalized float in [0.316, 1.0]"
  (let [(t (/ (- x 1.0) (+ x 1.0)))
        (t2 (* t t))
        (s (+ 1.0 (* t2 (+ 0.33333333 (* t2 (+ 0.2 (* t2 0.14285714)))))))
        (ln-x (* 2.0 (* t s)))]
    (* 8.6858896 ln-x)))

(df compute-db-level [(energy Float)] -> Float
  :d "Converts RMS energy to decibels relative to full scale, clamped to [-70.0, 0.0]"
  (cond
    ((<= energy 0.000316227766) -70.0)
    ((>= energy 1.0) 0.0)
    ((< energy 0.001)
     (let [(x (* energy 1000.0))]
       (if (< x 0.3162277)
           (+ -70.0 (db-norm (* x 3.16227766)))
           (+ -60.0 (db-norm x)))))
    ((< energy 0.01)
     (let [(x (* energy 100.0))]
       (if (< x 0.3162277)
           (+ -50.0 (db-norm (* x 3.16227766)))
           (+ -40.0 (db-norm x)))))
    ((< energy 0.1)
     (let [(x (* energy 10.0))]
       (if (< x 0.3162277)
           (+ -30.0 (db-norm (* x 3.16227766)))
           (+ -20.0 (db-norm x)))))
    (:else
     (if (< energy 0.3162277)
         (+ -10.0 (db-norm (* energy 3.16227766)))
         (db-norm energy)))))

(df make-audio-frame [(frame-id Str) (samples (List Float)) (speech-threshold Float) (timestamp-ms Int64)] -> AudioFrame
  :d "Instantiates AudioFrame calculating RMS energy, clamped dB level, and speech flag comparison"
  (let [(cnt (list-length samples))
        (rms (compute-frame-energy samples))
        (db (compute-db-level rms))
        (speech? (if (< speech-threshold 0.0)
                     (> db speech-threshold)
                     (> rms speech-threshold)))]
    (AudioFrame
      :frame-id frame-id
      :samples-count cnt
      :duration-ms 30
      :rms-energy rms
      :db-level db
      :is-speech speech?
      :timestamp-ms timestamp-ms)))

(df make-preroll-buffer [(capacity Int64)] -> PreRollBuffer
  :d "Constructs an empty pre-roll ring buffer with designated capacity"
  (PreRollBuffer
    :capacity capacity
    :frames (list)
    :count 0
    :head 0))

(df push-preroll-frame [(buf PreRollBuffer) (frame AudioFrame)] -> PreRollBuffer
  :d "Enqueues an audio frame; when capacity is reached, evicts oldest frame in FIFO order"
  (let [(cap (.-capacity buf))
        (cur-frames (.-frames buf))
        (cur-count (.-count buf))]
    (if (< cur-count cap)
        (PreRollBuffer
          :capacity cap
          :frames (list-append cur-frames (list frame))
          :count (+ cur-count 1)
          :head 0)
        (let [(evicted (option-or (list-tail cur-frames) (list)))]
          (PreRollBuffer
            :capacity cap
            :frames (list-append evicted (list frame))
            :count cap
            :head 0)))))

(df drain-preroll-frames [(buf PreRollBuffer)] -> (List AudioFrame)
  :d "Flushes all preserved pre-trigger onset frames in chronological order"
  (.-frames buf))

(df track-noise-floor [(current-floor-db Float) (frame-db Float) (is-speech Bool)] -> Float
  :d "Updates running background noise floor when is-speech is false; freezes when true; clamps to [-70.0, -25.0]"
  (if is-speech
      (if (< current-floor-db -70.0)
          -70.0
          (if (> current-floor-db -25.0)
              -25.0
              current-floor-db))
      (let [(updated (+ (* current-floor-db 0.9) (* frame-db 0.1)))]
        (if (< updated -70.0)
            -70.0
            (if (> updated -25.0)
                -25.0
                updated)))))
