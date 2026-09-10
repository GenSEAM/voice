(module asl-voice/aloud
  :d "Ear-friendly speech output formatter: strips visual chrome, expands duration units, sanitizes chips and enforces 8-second speech budget."
  :x [AloudConfig AloudSpeechResult
      make-aloud-config
      strip-visual-chrome
      expand-spoken-durations
      sanitize-chip-for-ear
      format-for-ear]
  :i [(core/strings :a s)])

(dfs AloudConfig
  (:f max-seconds Int64 "Maximum duration of spoken response in seconds")
  (:f chars-per-second Int64 "Estimated speaking rate in characters per second")
  (:f max-chars Int64 "Hard character limit for synthesized speech"))

(dfs AloudSpeechResult
  (:f spoken-text Str "Sanitized and clipped text ready for TTS")
  (:f char-count Int64 "Length of synthesized string")
  (:f estimated-duration-seconds Float "Estimated spoken duration in seconds")
  (:f is-truncated Bool "True if response was clipped to fit duration budget"))

(df make-aloud-config [(max-sec Int64) (rate-cps Int64)] -> AloudConfig
  :d "Constructs an AloudConfig record"
  (AloudConfig
    :max-seconds max-sec
    :chars-per-second rate-cps
    :max-chars (* max-sec rate-cps)))

(df strip-visual-chrome [(text Str)] -> Str
  :d "Removes terminal ANSI codes, Markdown formatting, brackets and hashes"
  (let [(c1 (string-replace text "**" ""))
        (c2 (string-replace c1 "*" ""))
        (c3 (string-replace c2 "`" ""))
        (c4 (string-replace c3 "#" ""))
        (c5 (string-replace c4 "[" ""))
        (c6 (string-replace c5 "]" ""))
        (c7 (string-replace c6 "{" ""))
        (c8 (string-replace c7 "}" ""))
        (c9 (string-replace c8 "·" " — "))
        (c10 (string-replace c9 "◆" ""))
        (c11 (string-replace c10 ">" ""))
        (c12 (string-replace c11 "<" ""))]
    (string-trim c12)))

(df expand-spoken-durations [(text Str)] -> Str
  :d "Expands duration shorthand units into fully articulated spoken words"
  (let [(t1 (string-replace text " 1m" " 1 minute"))
        (t2 (string-replace t1 " 2m" " 2 minutes"))
        (t3 (string-replace t2 " 3m" " 3 minutes"))
        (t4 (string-replace t3 " 5m" " 5 minutes"))
        (t5 (string-replace t4 " 10m" " 10 minutes"))
        (t6 (string-replace t5 " 15m" " 15 minutes"))
        (t7 (string-replace t6 " 30m" " 30 minutes"))
        (t8 (string-replace t7 " 45m" " 45 minutes"))
        (t9 (string-replace t8 " 1h" " 1 hour"))
        (t10 (string-replace t9 " 2h" " 2 hours"))
        (t11 (string-replace t10 " 1s" " 1 second"))
        (t12 (string-replace t11 " 2s" " 2 seconds"))
        (t13 (string-replace t12 " 5s" " 5 seconds"))
        (t14 (string-replace t13 " 10s" " 10 seconds"))
        (t15 (string-replace t14 " 30s" " 30 seconds"))]
    t15))

(df sanitize-chip-for-ear [(chip Str)] -> Str
  :d "Transforms repository chips and task references into speakable words"
  (let [(c1 (string-replace chip "·" " "))
        (c2 (string-replace c1 "." " "))
        (c3 (string-replace c2 "/" " "))
        (c4 (string-replace c3 "_" " "))
        (c5 (string-replace c4 "-" " "))]
    (string-trim c5)))

(df format-for-ear [(raw-response Str) (cfg AloudConfig)] -> AloudSpeechResult
  :d "Formats an agent response for ear consumption within strict speech duration limits"
  (let [(stripped (strip-visual-chrome raw-response))
        (expanded (expand-spoken-durations stripped))
        (budget (.-max-chars cfg))
        (len (string-length expanded))]
    (if (<= len budget)
      (AloudSpeechResult
        :spoken-text expanded
        :char-count len
        :estimated-duration-seconds (/ (float len) (float (.-chars-per-second cfg)))
        :is-truncated false)
      (let [(clipped (string-slice expanded 0 (- budget 1)))
            (final-text (str clipped "…"))
            (final-len (string-length final-text))]
        (AloudSpeechResult
          :spoken-text final-text
          :char-count final-len
          :estimated-duration-seconds (/ (float final-len) (float (.-chars-per-second cfg)))
          :is-truncated true)))))
