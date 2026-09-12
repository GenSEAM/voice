(module asl-voice/harness-profile
  :d "Local Harness Workstation Profile: Bilingual Russian-English phonetic alias dictionary, hardware endpoints, and private overlay"
  :x [HarnessPhoneticAlias
      HarnessHardwareEndpoint
      HarnessProfile
      make-phonetic-alias
      make-hardware-endpoint
      make-default-harness-profile
      make-default-hyness-profile
      add-phonetic-alias
      add-hardware-endpoint
      resolve-harness-alias
      apply-harness-aliases
      find-endpoint-by-kind
      format-harness-profile-asn
      format-hyness-profile-asn]
  :i [(asl-text/string :a s)])

(dfs HarnessPhoneticAlias
  (:f spoken Str "Spoken Cyrillic phonetic term e.g. асекс")
  (:f maps-to Str "Authoritative English codebase symbol e.g. genseam"))

(dfs HarnessHardwareEndpoint
  (:f kind Str "Endpoint category e.g. input, output, cdp, slm, hub")
  (:f name Str "Human-readable device or server title")
  (:f endpoint Str "Hardware URI or network socket connection string"))

(dfs HarnessProfile
  (:f profile-name Str "Identifier for the local workstation installation")
  (:f primary-locale Str "Primary language code e.g. en")
  (:f secondary-locale Str "Secondary language code e.g. ru")
  (:f is-mixed-mode Bool "True if bilingual code-switching is enabled")
  (:f aliases (List HarnessPhoneticAlias) "Active phonetic substitution table")
  (:f endpoints (List HarnessHardwareEndpoint) "Local hardware and server endpoints")
  (:f speaker-voiceprint-path Str "Path to local speaker ONNX voice biometric model"))

(df make-phonetic-alias [(spoken Str) (maps-to Str)] -> HarnessPhoneticAlias
  :d "Constructs HarnessPhoneticAlias mapping"
  (HarnessPhoneticAlias
    :spoken spoken
    :maps-to maps-to))

(df make-hardware-endpoint [(kind Str) (name Str) (endpoint Str)] -> HarnessHardwareEndpoint
  :d "Constructs HarnessHardwareEndpoint specification"
  (HarnessHardwareEndpoint
    :kind kind
    :name name
    :endpoint endpoint))

(df make-default-harness-profile [] -> HarnessProfile
  :d "Constructs default local Harness profile with canonical monorepo aliases and endpoints"
  (let [(a1 (make-phonetic-alias "асекс" "genseam"))
        (a2 (make-phonetic-alias "хайнесс" "harness"))
        (a3 (make-phonetic-alias "шроуди" "shrody"))
        (a4 (make-phonetic-alias "датахаб" "datahub"))
        (a5 (make-phonetic-alias "пцп" "pcp"))
        (a6 (make-phonetic-alias "ханес" "harness"))
        (aliases (list a1 a2 a3 a4 a5 a6))
        (ep-in (make-hardware-endpoint "input" "Default Microphone" "coreaudio://default-input"))
        (ep-out (make-hardware-endpoint "output" "Default Speaker" "coreaudio://default-output"))
        (ep-cdp (make-hardware-endpoint "cdp" "Chrome DevTools" "ws://127.0.0.1:9222/devtools/browser"))
        (ep-slm (make-hardware-endpoint "slm" "Local MLX Qwen" "http://127.0.0.1:8081/v1/chat/completions"))
        (ep-hub (make-hardware-endpoint "hub" "ASL Domain Socket Hub" "~/.asl/run/hub.sock"))
        (endpoints (list ep-in ep-out ep-cdp ep-slm ep-hub))]
    (HarnessProfile
      :profile-name "harness-workstation"
      :primary-locale "en"
      :secondary-locale "ru"
      :is-mixed-mode true
      :aliases aliases
      :endpoints endpoints
      :speaker-voiceprint-path "~/.asl/harness/speaker_voiceprint.onnx")))

(df make-default-hyness-profile [] -> HarnessProfile
  :d "Backward compatibility alias for make-default-harness-profile"
  (make-default-harness-profile))

(df add-phonetic-alias [(prof HarnessProfile) (alias HarnessPhoneticAlias)] -> HarnessProfile
  :d "Appends a new phonetic alias mapping to the Harness profile"
  (let [(updated (list-append (.-aliases prof) (list alias)))]
    (HarnessProfile
      :profile-name (.-profile-name prof)
      :primary-locale (.-primary-locale prof)
      :secondary-locale (.-secondary-locale prof)
      :is-mixed-mode (.-is-mixed-mode prof)
      :aliases updated
      :endpoints (.-endpoints prof)
      :speaker-voiceprint-path (.-speaker-voiceprint-path prof))))

(df add-hardware-endpoint [(prof HarnessProfile) (ep HarnessHardwareEndpoint)] -> HarnessProfile
  :d "Appends a new hardware or server endpoint to the Harness profile"
  (let [(updated (list-append (.-endpoints prof) (list ep)))]
    (HarnessProfile
      :profile-name (.-profile-name prof)
      :primary-locale (.-primary-locale prof)
      :secondary-locale (.-secondary-locale prof)
      :is-mixed-mode (.-is-mixed-mode prof)
      :aliases (.-aliases prof)
      :endpoints updated
      :speaker-voiceprint-path (.-speaker-voiceprint-path prof))))

(df resolve-harness-alias [(prof HarnessProfile) (word Str)] -> Str
  :d "Resolves a single spoken word against the Harness alias table or returns word unchanged"
  (let [(matches (list-filter (fn [(a HarnessPhoneticAlias)] (= (string-lower (.-spoken a)) (string-lower word))) (.-aliases prof)))]
    (if (list-empty? matches)
      word
      (.-maps-to (list-first matches)))))

(df apply-harness-aliases [(prof HarnessProfile) (text Str)] -> Str
  :d "Applies all defined phonetic aliases in sequence to normalize speech transcript"
  (list-fold
    (fn [(acc Str) (alias HarnessPhoneticAlias)]
      (string-replace acc (.-spoken alias) (.-maps-to alias)))
    text
    (.-aliases prof)))

(df find-endpoint-by-kind [(prof HarnessProfile) (target-kind Str)] -> Str
  :d "Looks up endpoint URI by kind with fallback to empty string"
  (let [(matches (list-filter (fn [(ep HarnessHardwareEndpoint)] (= (.-kind ep) target-kind)) (.-endpoints prof)))]
    (if (list-empty? matches)
      ""
      (.-endpoint (list-first matches)))))

(df format-harness-profile-asn [(prof HarnessProfile)] -> Str
  :d "Renders high-density ASN S-expression representing the Harness workstation configuration"
  (let [(a-len (string-from-int64 (list-length (.-aliases prof))))
        (e-len (string-from-int64 (list-length (.-endpoints prof))))
        (mm-str (if (.-is-mixed-mode prof) "true" "false"))]
    (str "(:harness-profile :name \"" (.-profile-name prof) "\" :primary \"" (.-primary-locale prof) "\" :secondary \"" (.-secondary-locale prof) "\" :mixed-mode " mm-str " :alias-count " a-len " :endpoint-count " e-len ")")))

(df format-hyness-profile-asn [(prof HarnessProfile)] -> Str
  :d "Backward compatibility alias for format-harness-profile-asn"
  (format-harness-profile-asn))
