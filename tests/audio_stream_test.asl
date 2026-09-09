(module asl-voice/tests/audio-stream-test
  :d "Dual-case unit tests for AudioStream circular PCM buffer and windowed slicing"
  :x [run-tests
      test-make-audio-stream
      test-stream-write-pcm
      test-stream-ring-overflow
      test-stream-read-window
      test-stream-is-full]
  :i [(audio_stream :a as)])

(df test-make-audio-stream [] -> Bool
  :d "Verifies AudioStream construction and default field state"
  (let [(st (as/make-audio-stream 512 16000))]
    (assert (= (.-capacity st) 512) "Capacity must equal 512")
    (assert (= (.-sample-rate st) 16000) "Sample rate must equal 16000")
    (assert (= (as/stream-buffer-size st) 0) "Initial buffer size must equal 0")
    (assert (not (as/stream-is-full? st)) "Initial buffer must not be full")
    true))

(df test-stream-write-pcm [] -> Bool
  :d "Verifies writing PCM chunks and cumulative byte accounting"
  (let [(st0 (as/make-audio-stream 10 16000))
        (chunk1 (list 0.1 0.2 0.3))
        (st1 (as/stream-write-pcm st0 chunk1))
        (chunk2 (list 0.4 0.5))
        (st2 (as/stream-write-pcm st1 chunk2))]
    (assert (= (as/stream-buffer-size st1) 3) "Buffer size after first write must equal 3")
    (assert (= (.-total-written st1) 3) "Total written after first write must equal 3")
    (assert (= (as/stream-buffer-size st2) 5) "Buffer size after second write must equal 5")
    (assert (= (.-total-written st2) 5) "Total written after second write must equal 5")
    true))

(df test-stream-ring-overflow [] -> Bool
  :d "Verifies FIFO eviction when ring buffer reaches capacity"
  (let [(st0 (as/make-audio-stream 4 16000))
        (st1 (as/stream-write-pcm st0 (list 1.0 2.0 3.0)))
        (st2 (as/stream-write-pcm st1 (list 4.0 5.0)))
        (win (as/stream-read-window st2 4))]
    (assert (not (as/stream-is-full? st1)) "st1 must not be full before overflow")
    (assert (as/stream-is-full? st2) "st2 must be full after overflow")
    (assert (= (as/stream-buffer-size st2) 4) "Buffer size must clamp to capacity 4")
    (assert (= (.-total-written st2) 5) "Total written must preserve cumulative count 5")
    (assert (= (list-length win) 4) "Window length must equal 4")
    true))

(df test-stream-read-window [] -> Bool
  :d "Verifies windowed slicing across empty, partial, and full conditions"
  (let [(st0 (as/make-audio-stream 8 16000))
        (st1 (as/stream-write-pcm st0 (list 1.0 2.0 3.0 4.0 5.0)))
        (w3 (as/stream-read-window st1 3))
        (w10 (as/stream-read-window st1 10))
        (w0 (as/stream-read-window st1 0))]
    (assert (= (list-length w3) 3) "Read window 3 must return 3 samples")
    (assert (= (list-length w10) 5) "Read window exceeding buffer size must clamp to current size 5")
    (assert (list-empty? w0) "Read window of size 0 must return empty list")
    true))

(df test-stream-is-full [] -> Bool
  :d "Verifies boolean stream full boundary detection"
  (let [(st0 (as/make-audio-stream 3 16000))
        (st1 (as/stream-write-pcm st0 (list 0.1 0.2 0.3)))
        (st2 (as/stream-write-pcm st1 (list 0.4)))]
    (assert (not (as/stream-is-full? st0)) "Empty stream must not be full")
    (assert (as/stream-is-full? st1) "Stream matching capacity must be full")
    (assert (as/stream-is-full? st2) "Stream after overflow must remain full")
    (assert (= (as/stream-buffer-size st2) 3) "Overflowed stream size must remain 3")
    true))

(df run-tests [] -> Bool
  :d "Executes all audio stream unit tests"
  (do
    (assert (test-make-audio-stream) "test-make-audio-stream must pass")
    (assert (test-stream-write-pcm) "test-stream-write-pcm must pass")
    (assert (test-stream-ring-overflow) "test-stream-ring-overflow must pass")
    (assert (test-stream-read-window) "test-stream-read-window must pass")
    (assert (test-stream-is-full) "test-stream-is-full must pass")
    true))

(run-tests)
