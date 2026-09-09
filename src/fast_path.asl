(module asl-voice/fast-path
  :d "Deterministic Fast-Path Transpiler and Monorepo Symbol Lexicon Indexer"
  :x [is-valid-asn-input?
      build-symbol-lexicon
      transpile-fast-path]
  :i [])

(df is-valid-asn-input? [(input String)] -> Bool
  :d "Validates whether input is a well-formed balanced ASN S-expression"
  (let [(trimmed (string-trim input))]
    (if (string-empty? trimmed)
      false
      (if (and (string-starts-with? trimmed "(") (string-ends-with? trimmed ")"))
        (let [(bal (fold (fn [(acc Int64) (ch String)] (if (< acc 0) -999 (if (= ch "(") (+ acc 1) (if (= ch ")") (- acc 1) acc)))) 0 (string-chars trimmed)))]
          (= bal 0))
        false))))

(df build-symbol-lexicon [] -> (List String)
  :d "Constructs canonical monorepo AST symbol lexicon for homophone indexing"
  (list
    "string-starts-with?"
    "string-ends-with?"
    "string-contains?"
    "string-length"
    "string-trim"
    "string-replace"
    "asl-mem"
    "asl-voice"
    "asl-intel"
    "task-claim"
    "task-settle"
    "task-recover"
    "task-spawn"
    "VadConfig"
    "VoiceFrame"
    "VoiceIntent"
    "compute-energy"
    "step-vad"
    "is-speech-frame"
    "run-tests"
    "transpile-fast-path"
    "is-valid-asn-input?"
    "build-symbol-lexicon"
    "disambiguate-homophones"
    "emit-asn-intent"
    "transliterate-voice-prompt"))

(df transpile-fast-path [(input String)] -> String
  :d "Transpiles structured ASN, JSON, or YAML into canonical S-expression form"
  (let [(trimmed (string-trim input))]
    (if (is-valid-asn-input? trimmed)
      trimmed
      (if (and (string-starts-with? trimmed "{") (string-ends-with? trimmed "}"))
        (str-concat "(:json " (string-replace (string-replace trimmed "{" "") "}" "") ")")
        (if (string-starts-with? trimmed "---")
          (str-concat "(:yaml " (string-trim (string-replace trimmed "---" "")) ")")
          "")))))
