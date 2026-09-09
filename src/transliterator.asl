(module asl-voice/transliterator
  :d "Context-Primed Homophone Disambiguator and Voice Prompt Transliterator Engine"
  :x [PhoneticRule
      make-phonetic-rules
      disambiguate-homophones
      emit-asn-intent
      transliterate-voice-prompt]
  :i [(fast_path :a fp)])

(dfs PhoneticRule
  (:f pattern Str "Spoken phonetic error pattern")
  (:f target Str "Canonical monorepo symbol"))

(df make-phonetic-rules [] -> (List PhoneticRule)
  :d "Constructs table of context-primed speech-to-text phonetic repair rules"
  (list
    (PhoneticRule :pattern "string starts swiss" :target "string-starts-with?")
    (PhoneticRule :pattern "string start swiss" :target "string-starts-with?")
    (PhoneticRule :pattern "string starts with" :target "string-starts-with?")
    (PhoneticRule :pattern "string start with" :target "string-starts-with?")
    (PhoneticRule :pattern "string ends swiss" :target "string-ends-with?")
    (PhoneticRule :pattern "string end swiss" :target "string-ends-with?")
    (PhoneticRule :pattern "string ends with" :target "string-ends-with?")
    (PhoneticRule :pattern "string end with" :target "string-ends-with?")
    (PhoneticRule :pattern "string contains" :target "string-contains?")
    (PhoneticRule :pattern "string contain" :target "string-contains?")
    (PhoneticRule :pattern "asl man" :target "asl-mem")
    (PhoneticRule :pattern "asl men" :target "asl-mem")
    (PhoneticRule :pattern "asl voice" :target "asl-voice")
    (PhoneticRule :pattern "asl boys" :target "asl-voice")
    (PhoneticRule :pattern "asl intel" :target "asl-intel")
    (PhoneticRule :pattern "task claim" :target "task-claim")
    (PhoneticRule :pattern "claim task" :target "task-claim")
    (PhoneticRule :pattern "task settle" :target "task-settle")
    (PhoneticRule :pattern "settle task" :target "task-settle")
    (PhoneticRule :pattern "task recover" :target "task-recover")
    (PhoneticRule :pattern "recover task" :target "task-recover")
    (PhoneticRule :pattern "task spawn" :target "task-spawn")
    (PhoneticRule :pattern "spawn task" :target "task-spawn")
    (PhoneticRule :pattern "vad config" :target "VadConfig")
    (PhoneticRule :pattern "bad config" :target "VadConfig")
    (PhoneticRule :pattern "voice frame" :target "VoiceFrame")
    (PhoneticRule :pattern "voice intent" :target "VoiceIntent")
    (PhoneticRule :pattern "compute energy" :target "compute-energy")
    (PhoneticRule :pattern "step vad" :target "step-vad")
    (PhoneticRule :pattern "step bad" :target "step-vad")
    (PhoneticRule :pattern "is speech frame" :target "is-speech-frame")
    (PhoneticRule :pattern "run tests" :target "run-tests")
    (PhoneticRule :pattern "run test" :target "run-tests")
    (PhoneticRule :pattern "fast path" :target "fast-path")))

(df disambiguate-homophones [(text String)] -> String
  :d "Repairs spoken homophone slips and phonetic errors against monorepo symbol lexicon"
  (let [(rules (make-phonetic-rules))]
    (fold (fn [(acc String) (r PhoneticRule)]
            (string-replace acc (.-pattern r) (.-target r)))
          text
          rules)))

(df emit-asn-intent [(goal String) (actions (List String))] -> String
  :d "Synthesizes canonical ASN intent frame with goal and action DAG"
  (let [(clean-actions (if (list-empty? actions)
                         (list (str-concat "\"" goal "\""))
                         (map (fn [(a String)] (if (string-starts-with? a "\"") a (str-concat "\"" a "\""))) actions)))
        (actions-str (string-join clean-actions " "))]
    (str-concat "(:intent :goal \"" goal "\" :action-dag [" actions-str "])")))

(df transliterate-voice-prompt [(raw-prompt String)] -> String
  :d "Transliterates ambient voice input or raw prompt into canonical executable ASN form"
  (let [(trimmed (string-trim raw-prompt))]
    (if (fp/is-valid-asn-input? trimmed)
      (fp/transpile-fast-path trimmed)
      (let [(repaired (disambiguate-homophones trimmed))
            (lex (fp/build-symbol-lexicon))
            (matched-actions (filter (fn [(sym String)] (string-contains? repaired sym)) lex))
            (actions (if (list-empty? matched-actions) (list repaired) matched-actions))]
        (emit-asn-intent repaired actions)))))
