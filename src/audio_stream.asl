(module asl-voice/audio-stream
  :d "In-memory circular PCM audio stream buffer with windowed slicing and overflow protection"
  :x [AudioStream
      make-audio-stream
      stream-write-pcm
      stream-read-window
      stream-buffer-size
      stream-is-full?]
  :i [])

(dfs AudioStream
  (:f capacity Int64 "Maximum number of samples retained in circular buffer")
  (:f sample-rate Int64 "Audio sample rate in Hz")
  (:f samples (List Float) "Ordered PCM samples currently stored in buffer")
  (:f total-written Int64 "Cumulative sample count written to stream"))

(df drop-samples [(n Int64) (xs (List Float))] -> (List Float)
  :d "Drops the first n elements from a list of floats"
  (if (or (<= n 0) (list-empty? xs))
    xs
    (drop-samples (- n 1) (option-or (list-tail xs) (list)))))

(df make-audio-stream [(capacity Int64) (sample-rate Int64)] -> AudioStream
  :d "Constructs an empty circular PCM audio stream buffer with specified capacity and sample rate"
  (AudioStream
    :capacity capacity
    :sample-rate sample-rate
    :samples (list)
    :total-written 0))

(df stream-buffer-size [(st AudioStream)] -> Int64
  :d "Returns current number of stored PCM samples in stream"
  (list-length (.-samples st)))

(df stream-is-full? [(st AudioStream)] -> Bool
  :d "Returns true if current buffer size has reached capacity"
  (>= (list-length (.-samples st)) (.-capacity st)))

(df stream-write-pcm [(st AudioStream) (chunk (List Float))] -> AudioStream
  :d "Appends incoming PCM chunk into circular buffer, evicting oldest samples on overflow"
  (if (list-empty? chunk)
    st
    (let [(cur-samples (.-samples st))
          (cap (.-capacity st))
          (all-samples (list-append cur-samples chunk))
          (all-len (list-length all-samples))
          (excess (- all-len cap))
          (retained (if (> excess 0) (drop-samples excess all-samples) all-samples))
          (new-total (+ (.-total-written st) (list-length chunk)))]
      (AudioStream
        :capacity cap
        :sample-rate (.-sample-rate st)
        :samples retained
        :total-written new-total))))

(df stream-read-window [(st AudioStream) (window-size Int64)] -> (List Float)
  :d "Reads most recent window of PCM samples up to specified window size"
  (if (<= window-size 0)
    (list)
    (let [(cur (.-samples st))
          (cur-len (list-length cur))]
      (if (<= cur-len window-size)
        cur
        (drop-samples (- cur-len window-size) cur)))))
