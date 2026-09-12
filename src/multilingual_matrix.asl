(module asl-voice/multilingual-matrix
  :d "Dynamic Multilingual Profile Matrix, Code-Switching Detection, and Locale Configurations"
  :x [LocaleDialect MultilingualConfig LanguageDetectionResult
      detect-language-dialect
      make-multilingual-config
      resolve-speech-engine]
  :i [(asl-text/string :a s)])

(dfe LocaleDialect
  (:c locale-en-primary [] "International English primary with standard technical vocabulary")
  (:c locale-ru-native [] "Russian native syntax and vocabulary")
  (:c locale-mixed-codeswitch [] "Mixed developer speech: Russian syntax with English tech tokens")
  (:c locale-universal [] "Multilingual auto-detection with adaptive code-switching"))

(dfs MultilingualConfig
  (:f dialect LocaleDialect "Active locale dialect profile")
  (:f primary-language Str "Primary ISO language code: en or ru")
  (:f secondary-language Str "Secondary ISO language code: ru or en")
  (:f enable-phonetic-folding Bool "True if Cyrillic-to-Latin phonetic sound folding is enabled")
  (:f tts-voice-id Str "Voice speaker profile identifier")
  (:f fast-path-enabled Bool "True if sub-millisecond fast-path matching is enabled"))

(dfs LanguageDetectionResult
  (:f detected-dialect LocaleDialect "Evaluated language dialect")
  (:f has-cyrillic Bool "True if Cyrillic code points are present")
  (:f has-latin-code-tokens Bool "True if Latin technical tokens are present")
  (:f confidence F64 "Detection confidence score"))

(df contains-cyrillic? [(text Str)] -> Bool
  :d "Detects if text contains Cyrillic characters"
  (let [(chars (string-chars (string-lower text)))]
    (list-any?
      (fn [(c Str)]
        (or (string-contains? "абвгдеёжзийклмнопрстуфхцчшщъыьэюя" c)
            (string-contains? "ієїґў" c)))
      chars)))

(df contains-latin? [(text Str)] -> Bool
  :d "Detects if text contains Latin characters"
  (let [(chars (string-chars (string-lower text)))]
    (list-any?
      (fn [(c Str)]
        (string-contains? "abcdefghijklmnopqrstuvwxyz" c))
      chars)))

(df detect-language-dialect [(text Str)] -> LanguageDetectionResult
  :d "Detects whether text is pure English, pure Russian, or mixed developer code-switching"
  (let [(cyr (contains-cyrillic? text))
        (lat (contains-latin? text))]
    (cond
      ((and cyr lat)
       (LanguageDetectionResult
         :detected-dialect (locale-mixed-codeswitch)
         :has-cyrillic true
         :has-latin-code-tokens true
         :confidence 0.95))
      (cyr
       (LanguageDetectionResult
         :detected-dialect (locale-ru-native)
         :has-cyrillic true
         :has-latin-code-tokens false
         :confidence 0.98))
      (lat
       (LanguageDetectionResult
         :detected-dialect (locale-en-primary)
         :has-cyrillic false
         :has-latin-code-tokens true
         :confidence 0.99))
      (:else
       (LanguageDetectionResult
         :detected-dialect (locale-en-primary)
         :has-cyrillic false
         :has-latin-code-tokens false
         :confidence 0.50)))))

(df make-multilingual-config [(dialect LocaleDialect)] -> MultilingualConfig
  :d "Constructs a pre-calibrated MultilingualConfig for the specified dialect"
  (cond
    ((= dialect (locale-en-primary))
     (MultilingualConfig
       :dialect dialect
       :primary-language "en"
       :secondary-language "ru"
       :enable-phonetic-folding false
       :tts-voice-id "en_0"
       :fast-path-enabled true))
    ((= dialect (locale-mixed-codeswitch))
     (MultilingualConfig
       :dialect dialect
       :primary-language "en"
       :secondary-language "ru"
       :enable-phonetic-folding true
       :tts-voice-id "xenia"
       :fast-path-enabled true))
    ((= dialect (locale-ru-native))
     (MultilingualConfig
       :dialect dialect
       :primary-language "ru"
       :secondary-language "en"
       :enable-phonetic-folding true
       :tts-voice-id "xenia"
       :fast-path-enabled true))
    (:else
     (MultilingualConfig
       :dialect (locale-universal)
       :primary-language "en"
       :secondary-language "ru"
       :enable-phonetic-folding true
       :tts-voice-id "en_0"
       :fast-path-enabled true))))

(df resolve-speech-engine [(cfg MultilingualConfig) (detection LanguageDetectionResult)] -> Str
  :d "Selects the optimal TTS engine and voice profile based on multilingual config and detection"
  (if (.-has-cyrillic detection)
    "silero-ru"
    (if (= (.-primary-language cfg) "en")
      "silero-en"
      "silero-ru")))
