(module asl-voice/hyness-profile
  :d "Local Hyness Workstation Profile: Bilingual Russian-English phonetic alias dictionary, hardware endpoints, and private overlay"
  :x [HynessPhoneticAlias
      HynessHardwareEndpoint
      HynessProfile
      make-phonetic-alias
      make-hardware-endpoint
      make-default-hyness-profile
      add-phonetic-alias
      add-hardware-endpoint
      resolve-hyness-alias
      apply-hyness-aliases
      find-endpoint-by-kind
      format-hyness-profile-asn]
  :i [(core/strings :a s)])

(dfs HynessPhoneticAlias
  (:f spoken Str "Spoken Cyrillic phonetic term e.g. асекс")
  (:f maps-to Str "Authoritative English codebase symbol e.g. asex"))

(dfs HynessHardwareEndpoint
  (:f kind Str "Endpoint category e.g. input, output, cdp, slm, hub")
  (:f name Str "Human-readable device or server title")
  (:f endpoint Str "Hardware URI or network socket connection string"))

(dfs HynessProfile
  (:f profile-name Str "Identifier for the local workstation installation")
  (:f primary-locale Str "Primary language code e.g. en")
  (:f secondary-locale Str "Secondary language code e.g. ru")
  (:f is-mixed-mode Bool "True if bilingual code-switching is enabled")
  (:f aliases (List HynessPhoneticAlias) "Active phonetic substitution table")
  (:f endpoints (List HynessHardwareEndpoint) "Local hardware and server endpoints")
  (:f speaker-voiceprint-path Str "Path to local speaker ONNX voice biometric model"))

(df make-phonetic-alias [(spoken Str) (maps-to Str)] -> HynessPhoneticAlias
  :d "Constructs HynessPhoneticAlias mapping"
  (HynessPhoneticAlias
    :spoken spoken
    :maps-to maps-to))

(df make-hardware-endpoint [(kind Str) (name Str) (endpoint Str)] -> HynessHardwareEndpoint
  :d "Constructs HynessHardwareEndpoint specification"
  (HynessHardwareEndpoint
    :kind kind
    :name name
    :endpoint endpoint))

(df make-default-hyness-profile [] -> HynessProfile
  :d "Constructs default local Hyness profile with canonical monorepo aliases and endpoints"
  (let [(a1 (make-phonetic-alias "асекс" "asex"))
        (a2 (make-phonetic-alias "хайнесс" "hyness"))
        (a3 (make-phonetic-alias "шроуди" "shrody"))
        (a4 (make-phonetic-alias "датахаб" "datahub"))
        (a5 (make-phonetic-alias "пцп" "pcp"))
        (aliases (list a1 a2 a3 a4 a5))
        (ep-in (make-hardware-endpoint "input" "Default Microphone" "coreaudio://default-input"))
        (ep-out (make-hardware-endpoint "output" "Default Speaker" "coreaudio://default-output"))
        (ep-cdp (make-hardware-endpoint "cdp" "Chrome DevTools" "ws://127.0.0.1:9222/devtools/browser"))
        (ep-slm (make-hardware-endpoint "slm" "Local MLX Qwen" "http://127.0.0.1:8081/v1/chat/completions"))
        (ep-hub (make-hardware-endpoint "hub" "ASL Domain Socket Hub" "~/.asl/run/hub.sock"))
        (endpoints (list ep-in ep-out ep-cdp ep-slm ep-hub))]
    (HynessProfile
      :profile-name "hyness-workstation"
      :primary-locale "en"
      :secondary-locale "ru"
      :is-mixed-mode true
      :aliases aliases
      :endpoints endpoints
      :speaker-voiceprint-path "~/.asl/hyness/speaker_voiceprint.onnx")))

(df add-phonetic-alias [(prof HynessProfile) (alias HynessPhoneticAlias)] -> HynessProfile
  :d "Appends a new phonetic alias mapping to the Hyness profile"
  (let [(updated (list-append (.-aliases prof) (list alias)))]
    (HynessProfile
      :profile-name (.-profile-name prof)
      :primary-locale (.-primary-locale prof)
      :secondary-locale (.-secondary-locale prof)
      :is-mixed-mode (.-is-mixed-mode prof)
      :aliases updated
      :endpoints (.-endpoints prof)
      :speaker-voiceprint-path (.-speaker-voiceprint-path prof))))

(df add-hardware-endpoint [(prof HynessProfile) (ep HynessHardwareEndpoint)] -> HynessProfile
  :d "Appends a new hardware or server endpoint to the Hyness profile"
  (let [(updated (list-append (.-endpoints prof) (list ep)))]
    (HynessProfile
      :profile-name (.-profile-name prof)
      :primary-locale (.-primary-locale prof)
      :secondary-locale (.-secondary-locale prof)
      :is-mixed-mode (.-is-mixed-mode prof)
      :aliases (.-aliases prof)
      :endpoints updated
      :speaker-voiceprint-path (.-speaker-voiceprint-path prof))))

(df resolve-hyness-alias [(prof HynessProfile) (word Str)] -> Str
  :d "Resolves a single spoken word against the Hyness alias table or returns word unchanged"
  (let [(matches (list-filter (fn [(a HynessPhoneticAlias)] (= (string-lower (.-spoken a)) (string-lower word))) (.-aliases prof)))]
    (if (list-empty? matches)
      word
      (.-maps-to (list-first matches)))))

(df apply-hyness-aliases [(prof HynessProfile) (text Str)] -> Str
  :d "Applies all defined phonetic aliases in sequence to normalize speech transcript"
  (list-fold
    (fn [(acc Str) (alias HynessPhoneticAlias)]
      (string-replace acc (.-spoken alias) (.-maps-to alias)))
    text
    (.-aliases prof)))

(df find-endpoint-by-kind [(prof HynessProfile) (target-kind Str)] -> Str
  :d "Looks up endpoint URI by kind with fallback to empty string"
  (let [(matches (list-filter (fn [(ep HynessHardwareEndpoint)] (= (.-kind ep) target-kind)) (.-endpoints prof)))]
    (if (list-empty? matches)
      ""
      (.-endpoint (list-first matches)))))

(df format-hyness-profile-asn [(prof HynessProfile)] -> Str
  :d "Renders high-density ASN S-expression representing the Hyness workstation configuration"
  (let [(a-len (string-from-int64 (list-length (.-aliases prof))))
        (e-len (string-from-int64 (list-length (.-endpoints prof))))
        (mm-str (if (.-is-mixed-mode prof) "true" "false"))]
    (str "(:hyness-profile :name \"" (.-profile-name prof) "\" :primary \"" (.-primary-locale prof) "\" :secondary \"" (.-secondary-locale prof) "\" :mixed-mode " mm-str " :alias-count " a-len " :endpoint-count " e-len ")")))
