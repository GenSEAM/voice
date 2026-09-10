(module asl-voice/intent-cleaner
  :d "Acoustic Disfluency Cleaner, Crosstalk Stripper, ASR Repair, and Core Intent Extractor in ASL"
  :x [CleanedIntentResult
      strip-disfluencies
      deduplicate-consecutive-tokens
      repair-asr-slips
      strip-conversational-preamble
      clean-voice-intent]
  :i [(core/strings :a s)])

(dfs CleanedIntentResult
  (:f raw-input Str "Original speech transcript")
  (:f cleaned-intent Str "Normalized and focused core intent")
  (:f stripped-fillers (List Str) "List of identified filler particles")
  (:f had-repetitions Bool "True if consecutive stutters were pruned")
  (:f repaired-tokens (List Str) "List of repaired ASR phonetic tokens"))

(df strip-disfluencies [(text Str)] -> Str
  :d "Strips hesitation particles and filler words in Russian and English"
  (let [(t1 (string-replace text "ну " ""))
        (t2 (string-replace t1 "эээ " ""))
        (t3 (string-replace t2 "ээ " ""))
        (t4 (string-replace t3 "ммм " ""))
        (t5 (string-replace t4 "типа " ""))
        (t6 (string-replace t5 "короче " ""))
        (t7 (string-replace t6 "в общем " ""))
        (t8 (string-replace t7 "как бы " ""))
        (t9 (string-replace t8 "значит " ""))
        (t10 (string-replace t9 "слушай " ""))
        (t11 (string-replace t10 "смотри " ""))
        (t12 (string-replace t11 "um " ""))
        (t13 (string-replace t12 "uh " ""))
        (t14 (string-replace t13 "hmm " ""))
        (t15 (string-replace t14 "like " ""))
        (t16 (string-replace t15 "you know " ""))
        (t17 (string-replace t16 "basically " ""))
        (t18 (string-replace t17 "actually " ""))]
    (string-trim t18)))

(df deduplicate-consecutive-tokens [(text Str)] -> Str
  :d "Removes immediate consecutive word stutters and repetitions"
  (let [(words (string-split (string-trim text) " "))]
    (if (<= (list-length words) 1)
      text
      (let [(folded (list-fold
                      (fn [(acc (List Str)) (w Str)]
                        (if (list-empty? acc)
                          (list w)
                          (let [(last-w (list-last acc))]
                            (if (= (string-lower last-w) (string-lower w))
                              acc
                              (list-append acc (list w))))))
                      (list)
                      words))]
        (string-join folded " ")))))

(df repair-asr-slips [(text Str)] -> Str
  :d "Repairs phonetic ASR transcription errors for technical terms and codebase names"
  (let [(t1 (string-replace text "шроуди" "shrody"))
        (t2 (string-replace t1 "шроди" "shrody"))
        (t3 (string-replace t2 "датахаб" "datahub"))
        (t4 (string-replace t3 "дата хаб" "datahub"))
        (t5 (string-replace t4 "пцп" "pcp"))
        (t6 (string-replace t5 "асекс" "asex"))
        (t7 (string-replace t6 "асл" "asl"))
        (t8 (string-replace t7 "гет " "git "))
        (t9 (string-replace t8 "пуш " "push "))
        (t10 (string-replace t9 "коммит " "commit "))
        (t11 (string-replace t10 "мердж " "merge "))
        (t12 (string-replace t11 "билд " "build "))
        (t13 (string-replace t12 "чекер " "checker "))
        (t14 (string-replace t13 "тесты " "tests "))
        (t15 (string-replace t14 "воркспейс " "workspace "))]
    t15))

(df strip-conversational-preamble [(text Str)] -> Str
  :d "Removes conversational fluff and focuses on the imperative core directive"
  (let [(trimmed (string-trim text))
        (low (string-lower trimmed))]
    (cond
      ((string-starts-with? low "можешь пожалуйста ")
       (string-slice trimmed 18 (string-length trimmed)))
      ((string-starts-with? low "пожалуйста ")
       (string-slice trimmed 11 (string-length trimmed)))
      ((string-starts-with? low "я хочу чтобы ты ")
       (string-slice trimmed 16 (string-length trimmed)))
      ((string-starts-with? low "сделай так чтобы ")
       (string-slice trimmed 17 (string-length trimmed)))
      ((string-starts-with? low "could you please ")
       (string-slice trimmed 17 (string-length trimmed)))
      ((string-starts-with? low "please ")
       (string-slice trimmed 7 (string-length trimmed)))
      ((string-starts-with? low "i would like you to ")
       (string-slice trimmed 20 (string-length trimmed)))
      ((string-starts-with? low "i want you to ")
       (string-slice trimmed 14 (string-length trimmed)))
      (:else trimmed))))

(df clean-voice-intent [(raw-transcript Str)] -> CleanedIntentResult
  :d "Orchestrates full intent cleaning pipeline: disfluencies, stutter dedup, ASR repair, and core focus"
  (let [(no-fillers (strip-disfluencies raw-transcript))
        (no-stutters (deduplicate-consecutive-tokens no-fillers))
        (repaired (repair-asr-slips no-stutters))
        (core (strip-conversational-preamble repaired))]
    (CleanedIntentResult
      :raw-input raw-transcript
      :cleaned-intent (string-trim core)
      :stripped-fillers (list)
      :had-repetitions (not (= no-fillers no-stutters))
      :repaired-tokens (list))))
