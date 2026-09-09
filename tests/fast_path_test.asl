(module asl-voice/tests/fast-path-test
  :d "Dual-case unit tests for deterministic fast-path transpiler and symbol lexicon"
  :x [run-tests
      test-valid-asn-input
      test-invalid-asn-input
      test-symbol-lexicon
      test-fast-path-transpilation]
  :i [(fast_path :a fp)])

(df test-valid-asn-input [] -> Bool
  :d "Verifies detection of well-formed balanced ASN expressions"
  (let [(r1 (fp/is-valid-asn-input? "(:task :id t1)"))
        (r2 (fp/is-valid-asn-input? "   (module foo :x [bar])  "))
        (r3 (fp/is-valid-asn-input? "((nested) (expr))"))]
    (assert r1 "Standard ASN task expression must be valid")
    (assert r2 "Trimmed module expression must be valid")
    (assert (not (= r3 false)) "Nested expression must not be invalid")
    true))

(df test-invalid-asn-input [] -> Bool
  :d "Verifies rejection of invalid unbalanced or malformed input"
  (let [(r1 (fp/is-valid-asn-input? "unstructured text without parens"))
        (r2 (fp/is-valid-asn-input? "(unclosed expression"))
        (r3 (fp/is-valid-asn-input? ")(bad paren order)("))
        (r4 (fp/is-valid-asn-input? ""))]
    (assert (not r1) "Plain text must not be valid ASN")
    (assert (not r2) "Unclosed parens must not be valid ASN")
    (assert (not r3) "Inverted parens must not be valid ASN")
    (assert (= r4 false) "Empty string must be false")
    true))

(df test-symbol-lexicon [] -> Bool
  :d "Verifies symbol lexicon contains required monorepo symbols"
  (let [(lex (fp/build-symbol-lexicon))
        (len (list-length lex))
        (has-sw (list-contains? lex "string-starts-with?"))
        (has-mem (list-contains? lex "asl-mem"))
        (has-claim (list-contains? lex "task-claim"))
        (has-bogus (list-contains? lex "non-existent-symbol"))]
    (assert (> len 15) "Lexicon must contain at least 15 symbols")
    (assert has-sw "Lexicon must contain string-starts-with?")
    (assert has-mem "Lexicon must contain asl-mem")
    (assert has-claim "Lexicon must contain task-claim")
    (assert (not has-bogus) "Lexicon must not contain arbitrary symbols")
    true))

(df test-fast-path-transpilation [] -> Bool
  :d "Verifies pass-through for ASN and rejection/empty for unstructured"
  (let [(asn-in "(:task :id t1 :priority :high)")
        (asn-res (fp/transpile-fast-path asn-in))
        (raw-res (fp/transpile-fast-path "please transpile this prompt"))
        (json-res (fp/transpile-fast-path "{\"action\": \"claim\"}"))]
    (assert (= asn-res asn-in) "Valid ASN must pass through unchanged")
    (assert (= raw-res "") "Unstructured prompt must return empty string on fast path")
    (assert (string-starts-with? json-res "(:json") "JSON input must wrap in json form")
    (assert (not (= (string-length asn-res) 0)) "ASN fast path result must not be empty")
    true))

(df run-tests [] -> Bool
  :d "Executes fast-path test suite"
  (do
    (assert (test-valid-asn-input) "test-valid-asn-input must pass")
    (assert (test-invalid-asn-input) "test-invalid-asn-input must pass")
    (assert (test-symbol-lexicon) "test-symbol-lexicon must pass")
    (assert (test-fast-path-transpilation) "test-fast-path-transpilation must pass")
    true))

(run-tests)
