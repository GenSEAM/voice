(module asl-voice/tests/transliterator-test
  :d "Dual-case unit tests for context-primed homophone disambiguator and voice prompt transliterator"
  :x [run-tests
      test-phonetic-repairs
      test-extended-homophones
      test-emit-asn-intent
      test-transliterate-voice-prompt
      test-fast-path-bypass]
  :i [(transliterator :a tr)
      (fast_path :a fp)])

(df test-phonetic-repairs [] -> Bool
  :d "Verifies phonetic disambiguation for primary monorepo homophone slips"
  (let [(r1 (tr/disambiguate-homophones "please use string start swiss on data"))
        (r2 (tr/disambiguate-homophones "connect to asl man context"))
        (r3 (tr/disambiguate-homophones "execute task claim for worker"))]
    (assert (string-contains? r1 "string-starts-with?") "Must repair string start swiss to string-starts-with?")
    (assert (not (string-contains? r1 "string start swiss")) "Must not retain corrupted phoneme string start swiss")
    (assert (string-contains? r2 "asl-mem") "Must repair asl man to asl-mem")
    (assert (not (string-contains? r2 "asl man")) "Must not retain corrupted phoneme asl man")
    (assert (string-contains? r3 "task-claim") "Must repair task claim to task-claim")
    (assert (not (string-contains? r3 "task claim")) "Must not retain corrupted phoneme task claim")
    true))

(df test-extended-homophones [] -> Bool
  :d "Verifies extended phonetic repairs for voice and VAD keywords"
  (let [(r1 (tr/disambiguate-homophones "initialize vad config and step vad"))
        (r2 (tr/disambiguate-homophones "run tests on asl voice"))
        (r3 (tr/disambiguate-homophones "execute task settle and task recover"))]
    (assert (string-contains? r1 "VadConfig") "Must repair vad config to VadConfig")
    (assert (string-contains? r1 "step-vad") "Must repair step vad to step-vad")
    (assert (string-contains? r2 "run-tests") "Must repair run tests to run-tests")
    (assert (string-contains? r2 "asl-voice") "Must repair asl voice to asl-voice")
    (assert (string-contains? r3 "task-settle") "Must repair task settle to task-settle")
    (assert (not (string-contains? r3 "task settle")) "Must not retain unhyphenated task settle")
    true))

(df test-emit-asn-intent [] -> Bool
  :d "Verifies synthesis of canonical ASN intent S-expressions"
  (let [(i1 (tr/emit-asn-intent "verify voice" (list "task-claim" "task-settle")))
        (i2 (tr/emit-asn-intent "standalone goal" (list)))
        (valid1 (fp/is-valid-asn-input? i1))
        (valid2 (fp/is-valid-asn-input? i2))]
    (assert valid1 "Emitted intent must be valid ASN")
    (assert valid2 "Empty action intent must be valid ASN")
    (assert (string-contains? i1 ":goal \"verify voice\"") "Intent must contain target goal")
    (assert (string-contains? i1 ":action-dag [\"task-claim\" \"task-settle\"]") "Intent must contain action DAG")
    (assert (not (= (string-length i1) 0)) "Emitted intent must not be empty")
    true))

(df test-transliterate-voice-prompt [] -> Bool
  :d "Verifies full pipeline from spoken transcript to canonical ASN intent"
  (let [(res1 (tr/transliterate-voice-prompt "string start swiss and asl man"))
        (res2 (tr/transliterate-voice-prompt "task claim then run tests and task settle"))
        (valid1 (fp/is-valid-asn-input? res1))
        (valid2 (fp/is-valid-asn-input? res2))]
    (assert valid1 "Transliterated output must be valid ASN S-expression")
    (assert valid2 "Multi-action transliterated output must be valid ASN S-expression")
    (assert (string-contains? res1 "string-starts-with?") "Must contain repaired string-starts-with?")
    (assert (string-contains? res1 "asl-mem") "Must contain repaired asl-mem")
    (assert (string-contains? res2 "task-claim") "Must contain task-claim action")
    (assert (string-contains? res2 "run-tests") "Must contain run-tests action")
    (assert (not (string-contains? res1 "asl man")) "Must not contain corrupted phoneme asl man")
    true))

(df test-fast-path-bypass [] -> Bool
  :d "Verifies that already valid ASN expressions bypass intent compilation"
  (let [(raw-asn "(:task :id t100 :priority :high)")
        (res (tr/transliterate-voice-prompt raw-asn))]
    (assert (= res raw-asn) "Valid ASN input must pass through unchanged without wrapping")
    (assert (not (string-starts-with? res "(:intent")) "Pre-structured ASN must not be wrapped in intent frame")
    true))

(df run-tests [] -> Bool
  :d "Executes transliterator test suite"
  (do
    (assert (test-phonetic-repairs) "test-phonetic-repairs must pass")
    (assert (test-extended-homophones) "test-extended-homophones must pass")
    (assert (test-emit-asn-intent) "test-emit-asn-intent must pass")
    (assert (test-transliterate-voice-prompt) "test-transliterate-voice-prompt must pass")
    (assert (test-fast-path-bypass) "test-fast-path-bypass must pass")
    true))

(run-tests)
