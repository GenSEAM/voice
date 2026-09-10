(module asl-voice/test-endturn-settle-aloud
  :d "Comprehensive unit and integration test suite for endturn, voice settle, aloud formatting, and tts engine."
  :x [test-endturn-analyzer
      test-voice-settle-lifecycle
      test-aloud-formatting
      test-tts-engine-routing
      run-all-voice-tests]
  :i [(endturn :a et)
      (voice_settle :a vs)
      (aloud :a al)
      (tts_engine :a tts)])

(df test-endturn-analyzer [] -> Bool
  (let [(r-dangling (et/analyze-turn-completion "создай тест для"))
        (r-hesitation (et/analyze-turn-completion "мы хотим ну"))
        (r-hanging (et/analyze-turn-completion "я хочу чтобы ты"))
        (r-aux (et/analyze-turn-completion "and now we should"))
        (r-complete-ru (et/analyze-turn-completion "добавь флаг версии в cli"))
        (r-complete-en (et/analyze-turn-completion "run the full test suite"))]
    (assert (not (.-is-complete r-dangling)) "trailing coordinator must not be complete")
    (assert (= (.-recommended-wait-ms r-dangling) 3000) "trailing coordinator must wait 3000ms")
    (assert (not (.-is-complete r-hesitation)) "trailing hesitation must not be complete")
    (assert (not (.-is-complete r-hanging)) "trailing subject must not be complete")
    (assert (not (.-is-complete r-aux)) "trailing aux verb must not be complete")
    (assert (.-is-complete r-complete-ru) "complete russian command must be complete")
    (assert (= (.-recommended-wait-ms r-complete-ru) 0) "complete russian command must wait 0ms")
    (assert (.-is-complete r-complete-en) "complete english command must be complete")
    true))

(df test-voice-settle-lifecycle [] -> Bool
  (let [(s0 (vs/make-initial-burst-state))
        (s1 (vs/ingest-transcript-fragment s0 "добавь флаг" 1000 3000 30000))
        (s2 (vs/ingest-transcript-fragment s1 "в cli" 2000 3000 30000))
        (step-early (vs/step-settle-timer s2 3500 30000))
        (step-timeout (vs/step-settle-timer s2 5500 30000))
        (s-long (vs/ingest-transcript-fragment s0 "длинный запрос" 1000 3000 30000))
        (step-ceiling (vs/step-settle-timer s-long 32000 30000))]
    (assert (.-is-active s1) "burst state active after first ingestion")
    (assert (= (.-message-count s2) 2) "burst message count is 2")
    (assert (= (.-accumulated-transcript s2) "добавь флаг в cli") "accumulated transcript concatenated")
    (assert (not (.-should-dispatch step-early)) "early step should not dispatch")
    (assert (.-should-dispatch step-timeout) "timeout step must dispatch")
    (assert (= (.-final-transcript step-timeout) "добавь флаг в cli") "dispatched transcript matches")
    (assert (.-should-dispatch step-ceiling) "ceiling step must dispatch")
    (assert (= (.-action step-ceiling) (vs/ceiling-reached)) "action is ceiling-reached")
    true))

(df test-aloud-formatting [] -> Bool
  (let [(cfg (al/make-aloud-config 8 14))
        (raw "**[QUEUED]** task #4f9c1a2b · 45m remaining · ◆ `vtbot`")
        (res (al/format-for-ear raw cfg))
        (spoken (.-spoken-text res))]
    (assert (not (string-contains? spoken "**")) "markdown asterisks stripped")
    (assert (not (string-contains? spoken "`")) "backticks stripped")
    (assert (not (string-contains? spoken "#")) "hash stripped")
    (assert (string-contains? spoken "45 minutes") "minutes expanded from 45m")
    (assert (<= (.-char-count res) 112) "spoken characters within budget")
    true))

(df test-tts-engine-routing [] -> Bool
  (let [(silero-cfg (tts/make-silero-config "ru" "xenia"))
        (kokoro-cfg (tts/make-kokoro-config "af_heart"))
        (opt-ru (tts/select-optimal-engine "ru" true false))
        (opt-en (tts/select-optimal-engine "en" true false))
        (job (tts/create-synthesis-job "j1" "Задача успешно выполнена" silero-cfg))]
    (assert (tts/supports-language? (tts/silero-onnx) "ru") "silero supports russian")
    (assert (tts/supports-language? (tts/silero-onnx) "en") "silero supports english")
    (assert (not (tts/supports-language? (tts/kokoro-onnx) "ru")) "kokoro does not support russian")
    (assert (= (.-engine opt-ru) (tts/silero-onnx)) "optimal engine for russian is silero")
    (assert (= (.-engine opt-en) (tts/silero-onnx)) "optimal engine with local silero is silero")
    (assert (= (.-status job) "completed") "synthesis job created with completed status")
    (assert (> (.-duration-ms job) 0) "job duration is positive")
    true))

(df run-all-voice-tests [] -> Bool
  (do
    (test-endturn-analyzer)
    (test-voice-settle-lifecycle)
    (test-aloud-formatting)
    (test-tts-engine-routing)
    true))

(run-all-voice-tests)
