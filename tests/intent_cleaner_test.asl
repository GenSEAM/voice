(module asl-voice/test-intent-cleaner
  :d "Unit tests for speech disfluency cleaning, stutter deduplication, ASR repair, and core intent extraction"
  :x [test-strip-disfluencies
      test-deduplicate-stutters
      test-repair-asr-slips
      test-strip-conversational-preamble
      test-clean-voice-intent-end-to-end
      run-all-intent-cleaner-tests]
  :i [(intent_cleaner :a ic)])

(df test-strip-disfluencies [] -> Bool
  (let [(r1 (ic/strip-disfluencies "ну эээ типа запусти тесты"))
        (r2 (ic/strip-disfluencies "um uh like run the build"))]
    (assert (= r1 "запусти тесты") "russian disfluencies stripped")
    (assert (= r2 "run the build") "english disfluencies stripped")
    true))

(df test-deduplicate-stutters [] -> Bool
  (let [(r1 (ic/deduplicate-consecutive-tokens "мы мы хотим запустить"))
        (r2 (ic/deduplicate-consecutive-tokens "run run the test test suite"))]
    (assert (= r1 "мы хотим запустить") "consecutive russian word stutters deduplicated")
    (assert (= r2 "run the test suite") "consecutive english word stutters deduplicated")
    true))

(df test-repair-asr-slips [] -> Bool
  (let [(r1 (ic/repair-asr-slips "запусти шроуди и датахаб"))
        (r2 (ic/repair-asr-slips "сделай гет пуш и коммит"))]
    (assert (string-contains? r1 "shrody") "шроуди repaired to shrody")
    (assert (string-contains? r1 "datahub") "датахаб repaired to datahub")
    (assert (string-contains? r2 "git") "гет repaired to git")
    (assert (string-contains? r2 "push") "пуш repaired to push")
    (assert (string-contains? r2 "commit") "коммит repaired to commit")
    true))

(df test-strip-conversational-preamble [] -> Bool
  (let [(r1 (ic/strip-conversational-preamble "можешь пожалуйста запустить проверку"))
        (r2 (ic/strip-conversational-preamble "could you please run the verification"))]
    (assert (= r1 "запустить проверку") "russian preamble stripped")
    (assert (= r2 "run the verification") "english preamble stripped")
    true))

(df test-clean-voice-intent-end-to-end [] -> Bool
  (let [(res (ic/clean-voice-intent "ну эээ можешь пожалуйста запустить запустить тесты в шроуди"))]
    (assert (.-had-repetitions res) "repetitions detected in raw transcript")
    (assert (= (.-cleaned-intent res) "запустить tests в shrody") "end-to-end cleaned intent matches expectation")
    true))

(df run-all-intent-cleaner-tests [] -> Bool
  (do
    (test-strip-disfluencies)
    (test-deduplicate-stutters)
    (test-repair-asr-slips)
    (test-strip-conversational-preamble)
    (test-clean-voice-intent-end-to-end)
    true))

(run-all-intent-cleaner-tests)
