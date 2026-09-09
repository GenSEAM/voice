(module asl-voice/tests/voice-intent-test
  :d "Dual-case unit tests for speech utterance dispatch, transliterator integration, and in-flight binding"
  :x [run-tests
      test-format-voice-intent-asn
      test-dispatch-speech-utterance
      test-dispatch-phonetic-homophones
      test-bind-intent-to-inflight
      test-fast-path-preservation]
  :i [(voice_intent :a vi)
      (fast_path :a fp)])

(df test-format-voice-intent-asn [] -> Bool
  :d "Verifies formatting of structured VoiceIntent ASN representations"
  (let [(r1 (vi/format-voice-intent-asn "verify voice pipeline" (list "task-claim" "task-settle")))
        (r2 (vi/format-voice-intent-asn "single action" (list "run-tests")))]
    (assert (fp/is-valid-asn-input? r1) "Formatted intent r1 must be valid balanced ASN")
    (assert (fp/is-valid-asn-input? r2) "Formatted intent r2 must be valid balanced ASN")
    (assert (string-contains? r1 ":goal \"verify voice pipeline\"") "r1 must contain target goal")
    (assert (string-contains? r1 ":action-dag [\"task-claim\" \"task-settle\"]") "r1 must contain action DAG")
    (assert (string-contains? r2 ":action-dag [\"run-tests\"]") "r2 must contain single action")
    true))

(df test-dispatch-speech-utterance [] -> Bool
  :d "Verifies speech utterance dispatch into structured intent frame"
  (let [(res1 (vi/dispatch-speech-utterance "task claim for worker"))
        (res2 (vi/dispatch-speech-utterance "run tests"))]
    (assert (fp/is-valid-asn-input? res1) "Dispatched utterance res1 must be valid ASN")
    (assert (fp/is-valid-asn-input? res2) "Dispatched utterance res2 must be valid ASN")
    (assert (string-contains? res1 ":action-dag [\"task-claim\"]") "res1 must contain task-claim in action-dag")
    (assert (string-contains? res2 ":action-dag [\"run-tests\"]") "res2 must contain run-tests in action-dag")
    true))

(df test-dispatch-phonetic-homophones [] -> Bool
  :d "Verifies that spoken homophone slips are disambiguated before intent creation"
  (let [(res1 (vi/dispatch-speech-utterance "string start swiss and asl man"))
        (res2 (vi/dispatch-speech-utterance "vad config then step vad"))]
    (assert (fp/is-valid-asn-input? res1) "Homophone repaired intent res1 must be valid ASN")
    (assert (fp/is-valid-asn-input? res2) "Homophone repaired intent res2 must be valid ASN")
    (assert (string-contains? res1 "string-starts-with?") "res1 must repair string start swiss to string-starts-with?")
    (assert (string-contains? res1 "asl-mem") "res1 must repair asl man to asl-mem")
    (assert (string-contains? res2 "VadConfig") "res2 must repair vad config to VadConfig")
    (assert (string-contains? res2 "step-vad") "res2 must repair step vad to step-vad")
    (assert (not (string-contains? res1 "string start swiss")) "res1 must not contain raw phonetic error")
    true))

(df test-bind-intent-to-inflight [] -> Bool
  :d "Verifies binding structured intent into Tier 3 in-flight task frame"
  (let [(intent (vi/format-voice-intent-asn "execute build" (list "task-spawn")))
        (bound1 (vi/bind-intent-to-inflight "t394-01" intent))
        (bound2 (vi/bind-intent-to-inflight "t394-02" "task-settle"))]
    (assert (fp/is-valid-asn-input? bound1) "Bound in-flight frame bound1 must be valid ASN")
    (assert (fp/is-valid-asn-input? bound2) "Bound in-flight frame bound2 must be valid ASN")
    (assert (string-contains? bound1 ":task-id \"t394-01\"") "bound1 must contain task ID")
    (assert (string-contains? bound1 ":status :active") "bound1 must contain active status")
    (assert (string-contains? bound2 ":task-id \"t394-02\"") "bound2 must contain task ID")
    (assert (string-contains? bound2 ":intent (:intent :goal \"task-settle\"") "bound2 must auto-wrap raw intent")
    true))

(df test-fast-path-preservation [] -> Bool
  :d "Verifies that already structured ASN expressions pass through unchanged"
  (let [(raw-asn "(:task :id \"t100\" :priority :high)")
        (dispatched (vi/dispatch-speech-utterance raw-asn))]
    (assert (= dispatched raw-asn) "Pre-structured ASN must bypass transliteration")
    (assert (fp/is-valid-asn-input? dispatched) "Bypassed expression must be valid ASN")
    true))

(df run-tests [] -> Bool
  :d "Executes all voice intent unit tests"
  (do
    (assert (test-format-voice-intent-asn) "test-format-voice-intent-asn must pass")
    (assert (test-dispatch-speech-utterance) "test-dispatch-speech-utterance must pass")
    (assert (test-dispatch-phonetic-homophones) "test-dispatch-phonetic-homophones must pass")
    (assert (test-bind-intent-to-inflight) "test-bind-intent-to-inflight must pass")
    (assert (test-fast-path-preservation) "test-fast-path-preservation must pass")
    true))

(run-tests)
