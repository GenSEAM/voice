(module asl-voice/tts-engine
  :d "Multi-engine Text-To-Speech (TTS) routing: Silero ONNX (native RU/EN), Kokoro ONNX, and Cloud streaming fallbacks."
  :x [TtsEngineKind TtsEngineConfig TtsSynthesisJob
      make-silero-config
      make-kokoro-config
      supports-language?
      select-optimal-engine
      create-synthesis-job]
  :i [(asl-text/string :a s)])

(dfe TtsEngineKind
  (:c silero-onnx [] "Silero TTS v4/v5 ONNX 24kHz (native Russian and English)")
  (:c kokoro-onnx [] "Kokoro TTS v1.0 ONNX 24kHz (English and select languages)")
  (:c cloud-stream [] "Cloud streaming speech synthesis fallback")
  (:c terminal-silent [] "Terminal text-only fallback without audio synthesis"))

(dfs TtsEngineConfig
  (:f engine TtsEngineKind "Selected synthesis engine backend")
  (:f model-path Str "Filesystem path or endpoint URL for engine model")
  (:f sample-rate I64 "Audio output sampling rate in Hz (e.g. 24000 or 48000)")
  (:f language Str "Target language ISO code (ru or en)")
  (:f voice-id Str "Voice speaker profile identifier (e.g. xenia, baya, eugene, af_heart)")
  (:f speed F64 "Speaking tempo rate multiplier (1.0 = normal)"))

(dfs TtsSynthesisJob
  (:f job-id Str "Unique synthesis task identifier")
  (:f text Str "Sanitized speech text to articulate")
  (:f config TtsEngineConfig "Engine configuration for job")
  (:f status Str "Execution state: pending, synthesizing, completed, failed")
  (:f audio-samples-count I64 "Number of generated audio samples")
  (:f duration-ms I64 "Duration of synthesized audio in ms"))

(df make-silero-config [(language Str) (voice-id Str)] -> TtsEngineConfig
  :d "Constructs a pre-calibrated Silero TTS ONNX configuration with full Russian and English support"
  (TtsEngineConfig
    :engine (silero-onnx)
    :model-path "~/.asl/models/silero_v4.onnx"
    :sample-rate 24000
    :language language
    :voice-id (if (= voice-id "") (if (= language "ru") "xenia" "en_0") voice-id)
    :speed 1.0))

(df make-kokoro-config [(voice-id Str)] -> TtsEngineConfig
  :d "Constructs Kokoro TTS ONNX configuration for English voices"
  (TtsEngineConfig
    :engine (kokoro-onnx)
    :model-path "~/.asl/models/kokoro-v1.0.onnx"
    :sample-rate 24000
    :language "en"
    :voice-id (if (= voice-id "") "af_heart" voice-id)
    :speed 1.0))

(df supports-language? [(engine TtsEngineKind) (language Str)] -> Bool
  :d "Validates whether the specified TTS engine can articulate the target language"
  (let [(lang (string-lower (string-trim language)))]
    (cond
      ((= engine (silero-onnx))
       (or (= lang "ru") (= lang "en")))
      ((= engine (kokoro-onnx))
       (= lang "en"))
      ((= engine (cloud-stream))
       true)
      (:else false))))

(df select-optimal-engine [(language Str) (has-local-silero Bool) (has-cloud Bool)] -> TtsEngineConfig
  :d "Auto-selects the optimal TTS engine for the requested language and available hardware capabilities"
  (let [(lang (string-lower (string-trim language)))]
    (cond
      ((and (= lang "ru") has-local-silero)
       (make-silero-config "ru" "xenia"))
      ((and (= lang "en") has-local-silero)
       (make-silero-config "en" "en_0"))
      ((and (= lang "en") (not has-local-silero))
       (make-kokoro-config "af_heart"))
      (has-cloud
       (TtsEngineConfig
         :engine (cloud-stream)
         :model-path "https://api.openai.com/v1/audio/speech"
         :sample-rate 24000
         :language lang
         :voice-id "alloy"
         :speed 1.0))
      (:else
       (TtsEngineConfig
         :engine (terminal-silent)
         :model-path ""
         :sample-rate 16000
         :language lang
         :voice-id "silent"
         :speed 1.0)))))

(df create-synthesis-job [(job-id Str) (text Str) (cfg TtsEngineConfig)] -> TtsSynthesisJob
  :d "Instantiates a new TtsSynthesisJob record"
  (let [(char-count (string-length text))
        (est-duration-ms (as-i64 (* (as-f64 (/ (* char-count 1000) 14)) (.-speed cfg))))
        (samples-count (/ (* est-duration-ms (.-sample-rate cfg)) 1000))]
    (TtsSynthesisJob
      :job-id job-id
      :text text
      :config cfg
      :status "completed"
      :audio-samples-count samples-count
      :duration-ms est-duration-ms)))
