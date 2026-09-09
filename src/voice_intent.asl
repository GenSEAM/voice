(module asl-voice/voice-intent
  :d "Speech-to-Intent pipeline dispatcher, transliterator integration, and Tier 3 in-flight binding"
  :x [dispatch-speech-utterance
      bind-intent-to-inflight
      format-voice-intent-asn]
  :i [(transliterator :a tr)
      (fast_path :a fp)])

(df format-voice-intent-asn [(goal Str) (actions (List Str))] -> Str
  :d "Formats a structured VoiceIntent frame into canonical ASN S-expression"
  (tr/emit-asn-intent goal actions))

(df dispatch-speech-utterance [(utterance Str)] -> Str
  :d "Routes speech utterance through context-primed transliterator into structured intent"
  (tr/transliterate-voice-prompt utterance))

(df bind-intent-to-inflight [(task-id Str) (intent-asn Str)] -> Str
  :d "Binds voice intent into Tier 3 in-flight task representation"
  (let [(clean-intent (if (string-starts-with? intent-asn "(")
                        intent-asn
                        (format-voice-intent-asn intent-asn (list intent-asn))))]
    (str-concat "(:in-flight :task-id \"" (str-concat task-id (str-concat "\" :intent " (str-concat clean-intent " :status :active)"))))))
