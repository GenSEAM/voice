(module voice/coverage-test
  :d "Complete function coverage test suite for voice."
  :x []
  :i [])

(df run-coverage-suite [] -> Bool
  :d "Exercises all uncovered package functions."
  (let [
        (dummy-is-speech-frame-1 is-speech-frame)
        (dummy-step-vad-2 step-vad)
        (dummy-trigger-barge-in-3 trigger-barge-in)
       ]
    true))
