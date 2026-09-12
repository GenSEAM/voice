(module asl-voice/voice-settle
  :d "Voice burst buffering and debounce state machine with quiet edge detection and ceiling enforcement."
  :x [SettleAction BurstState SettleStepResult
      make-initial-burst-state
      ingest-transcript-fragment
      step-settle-timer
      release-burst-now]
  :i [(asl-text/string :a s)])

(dfe SettleAction
  (:c hold-ongoing [] "Burst is accumulating speech fragments")
  (:c quiet-edge [] "Pause detected at quiet edge")
  (:c timeout-settled [] "Settle timer fired, releasing burst")
  (:c ceiling-reached [] "30s ceiling reached, force-releasing burst")
  (:c instant-release [] "Grammatical endturn triggered instant release"))

(dfs BurstState
  (:f is-active Bool "True if currently buffering a speech burst")
  (:f first-arrival-ms I64 "Timestamp of first message in burst")
  (:f last-arrival-ms I64 "Timestamp of latest fragment in burst")
  (:f due-at-ms I64 "Timestamp when debounce timer will fire")
  (:f message-count I64 "Count of speech fragments merged")
  (:f accumulated-transcript Str "Merged utterance transcript text")
  (:f is-held Bool "External speaker hold flag"))

(dfs SettleStepResult
  (:f action SettleAction "Classification of step transition")
  (:f next-state BurstState "Updated burst buffer state")
  (:f should-dispatch Bool "True if burst should be dispatched to pipeline")
  (:f final-transcript Str "Dispatched transcript payload")
  (:f held-duration-ms I64 "Total duration burst was held in ms"))

(df make-initial-burst-state [] -> BurstState
  :d "Constructs an idle burst buffer state"
  (BurstState
    :is-active false
    :first-arrival-ms 0
    :last-arrival-ms 0
    :due-at-ms 0
    :message-count 0
    :accumulated-transcript ""
    :is-held false))

(df ingest-transcript-fragment [(state BurstState) (fragment Str) (now-ms I64) (settle-ms I64) (max-hold-ms I64)] -> BurstState
  :d "Merges an incoming transcript fragment into the burst buffer and re-arms debounce deadline"
  (let [(clean (string-trim fragment))]
    (if (not (.-is-active state))
      (BurstState
        :is-active true
        :first-arrival-ms now-ms
        :last-arrival-ms now-ms
        :due-at-ms (+ now-ms settle-ms)
        :message-count 1
        :accumulated-transcript clean
        :is-held true)
      (let [(prev (.-accumulated-transcript state))
            (merged (if (= prev "") clean (str prev " " clean)))
            (ceiling (+ (.-first-arrival-ms state) max-hold-ms))
            (normal-due (+ now-ms settle-ms))
            (effective-due (if (> normal-due ceiling) ceiling normal-due))]
        (BurstState
          :is-active true
          :first-arrival-ms (.-first-arrival-ms state)
          :last-arrival-ms now-ms
          :due-at-ms effective-due
          :message-count (+ (.-message-count state) 1)
          :accumulated-transcript merged
          :is-held true)))))

(df step-settle-timer [(state BurstState) (now-ms I64) (max-hold-ms I64)] -> SettleStepResult
  :d "Evaluates running burst state against deadlines and produces settle action"
  (if (not (.-is-active state))
    (SettleStepResult
      :action (hold-ongoing)
      :next-state state
      :should-dispatch false
      :final-transcript ""
      :held-duration-ms 0)
    (let [(first-at (.-first-arrival-ms state))
          (held-ms (- now-ms first-at))
          (due-at (.-due-at-ms state))
          (transcript (.-accumulated-transcript state))]
      (cond
        ((>= held-ms max-hold-ms)
         (SettleStepResult
           :action (ceiling-reached)
           :next-state (make-initial-burst-state)
           :should-dispatch true
           :final-transcript transcript
           :held-duration-ms held-ms))
        ((>= now-ms due-at)
         (SettleStepResult
           :action (timeout-settled)
           :next-state (make-initial-burst-state)
           :should-dispatch true
           :final-transcript transcript
           :held-duration-ms held-ms))
        (:else
         (SettleStepResult
           :action (hold-ongoing)
           :next-state state
           :should-dispatch false
           :final-transcript ""
           :held-duration-ms held-ms))))))

(df release-burst-now [(state BurstState) (now-ms I64)] -> SettleStepResult
  :d "Immediately releases any buffered burst transcript without waiting for timeouts"
  (if (not (.-is-active state))
    (SettleStepResult
      :action (instant-release)
      :next-state state
      :should-dispatch false
      :final-transcript ""
      :held-duration-ms 0)
    (let [(held-ms (- now-ms (.-first-arrival-ms state)))
          (transcript (.-accumulated-transcript state))]
      (SettleStepResult
        :action (instant-release)
        :next-state (make-initial-burst-state)
        :should-dispatch true
        :final-transcript transcript
        :held-duration-ms held-ms))))
