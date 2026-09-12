(module asl-voice/endturn
  :d "Grammatical Turn Completion Analyzer for Voice Assistant: releases complete utterances instantly and holds incomplete thoughts."
  :x [TurnVerdict TurnAnalysis
      make-turn-analysis
      is-dangling-coordinator?
      is-hesitation-particle?
      is-hanging-subject-pronoun?
      is-dangling-aux-verb?
      extract-trailing-token
      analyze-turn-completion]
  :i [(asl-text/string :a s)])

(dfe TurnVerdict
  (:c complete [] "Utterance is grammatically complete")
  (:c dangling [] "Trailing preposition, subordinator or coordinator")
  (:c hesitation [] "Trailing filler or hesitation particle")
  (:c hanging-subject [] "Trailing subject pronoun without predicate")
  (:c dangling-aux [] "Trailing auxiliary verb waiting for main verb")
  (:c empty-utterance [] "Empty or whitespace utterance"))

(dfs TurnAnalysis
  (:f verdict TurnVerdict "Categorical verdict of turn state")
  (:f is-complete Bool "True if utterance can be dispatched immediately")
  (:f trailing-token Str "Extracted trailing word token")
  (:f reason Str "Human-readable diagnostic reason")
  (:f recommended-wait-ms I64 "Recommended debounce silence wait in ms"))

(df make-turn-analysis [(verdict TurnVerdict) (is-complete Bool) (trailing Str) (reason Str) (wait-ms I64)] -> TurnAnalysis
  :d "Constructs a TurnAnalysis record"
  (TurnAnalysis
    :verdict verdict
    :is-complete is-complete
    :trailing-token trailing
    :reason reason
    :recommended-wait-ms wait-ms))

(df is-dangling-coordinator? [(w Str)] -> Bool
  :d "Detects coordinators, subordinators and prepositions in Russian and English"
  (let [(low (string-lower (string-trim w)))]
    (cond
      ((= low "и") true)
      ((= low "а") true)
      ((= low "но") true)
      ((= low "или") true)
      ((= low "либо") true)
      ((= low "что") true)
      ((= low "чтобы") true)
      ((= low "чтоб") true)
      ((= low "если") true)
      ((= low "когда") true)
      ((= low "пока") true)
      ((= low "потому") true)
      ((= low "поэтому") true)
      ((= low "как") true)
      ((= low "для") true)
      ((= low "на") true)
      ((= low "в") true)
      ((= low "во") true)
      ((= low "с") true)
      ((= low "со") true)
      ((= low "к") true)
      ((= low "ко") true)
      ((= low "о") true)
      ((= low "об") true)
      ((= low "от") true)
      ((= low "до") true)
      ((= low "по") true)
      ((= low "за") true)
      ((= low "из") true)
      ((= low "при") true)
      ((= low "про") true)
      ((= low "без") true)
      ((= low "у") true)
      ((= low "and") true)
      ((= low "or") true)
      ((= low "but") true)
      ((= low "so") true)
      ((= low "that") true)
      ((= low "if") true)
      ((= low "when") true)
      ((= low "while") true)
      ((= low "because") true)
      ((= low "since") true)
      ((= low "to") true)
      ((= low "for") true)
      ((= low "with") true)
      ((= low "of") true)
      ((= low "in") true)
      ((= low "on") true)
      ((= low "at") true)
      ((= low "by") true)
      ((= low "from") true)
      ((= low "about") true)
      (:else false))))

(df is-hesitation-particle? [(w Str)] -> Bool
  :d "Detects hesitation sounds and filler particles in Russian and English"
  (let [(low (string-lower (string-trim w)))]
    (cond
      ((= low "ну") true)
      ((= low "вот") true)
      ((= low "это") true)
      ((= low "типа") true)
      ((= low "короче") true)
      ((= low "значит") true)
      ((= low "эээ") true)
      ((= low "ээ") true)
      ((= low "э") true)
      ((= low "ммм") true)
      ((= low "мм") true)
      ((= low "um") true)
      ((= low "uh") true)
      ((= low "erm") true)
      ((= low "hmm") true)
      ((= low "like") true)
      ((= low "well") true)
      (:else false))))

(df is-hanging-subject-pronoun? [(w Str)] -> Bool
  :d "Detects trailing nominative pronouns without predicates"
  (let [(low (string-lower (string-trim w)))]
    (cond
      ((= low "я") true)
      ((= low "ты") true)
      ((= low "он") true)
      ((= low "она") true)
      ((= low "оно") true)
      ((= low "мы") true)
      ((= low "вы") true)
      ((= low "они") true)
      ((= low "i") true)
      ((= low "he") true)
      ((= low "she") true)
      ((= low "we") true)
      ((= low "they") true)
      (:else false))))

(df is-dangling-aux-verb? [(w Str)] -> Bool
  :d "Detects trailing auxiliary and modal verbs waiting for action verbs"
  (let [(low (string-lower (string-trim w)))]
    (cond
      ((= low "should") true)
      ((= low "would") true)
      ((= low "could") true)
      ((= low "can") true)
      ((= low "will") true)
      ((= low "shall") true)
      ((= low "must") true)
      ((= low "might") true)
      ((= low "may") true)
      ((= low "gonna") true)
      ((= low "wanna") true)
      ((= low "want") true)
      ((= low "need") true)
      (:else false))))

(df extract-trailing-token [(text Str)] -> Str
  :d "Extracts and normalizes the last word token from an utterance"
  (let [(trimmed (string-trim text))]
    (if (= trimmed "")
      ""
      (let [(clean (string-replace trimmed "." ""))
            (clean2 (string-replace clean "," ""))
            (clean3 (string-replace clean2 "!" ""))
            (clean4 (string-replace clean3 "?" ""))
            (clean5 (string-replace clean4 ";" ""))
            (clean6 (string-replace clean5 ":" ""))
            (words (string-split clean6 " "))]
        (if (list-empty? words)
          ""
          (let [(last-word (option-or (list-last words) ""))]
            (string-lower (string-trim last-word))))))))

(df analyze-turn-completion [(utterance Str)] -> TurnAnalysis
  :d "Analyzes an utterance and determines whether it is complete or needs to wait for speaker continuation"
  (let [(token (extract-trailing-token utterance))]
    (cond
      ((= token "")
       (make-turn-analysis (empty-utterance) false "" "Empty utterance" 0))
      ((is-dangling-coordinator? token)
       (make-turn-analysis (dangling) false token "Trailing coordinator or preposition" 3000))
      ((is-hesitation-particle? token)
       (make-turn-analysis (hesitation) false token "Trailing hesitation filler" 3000))
      ((is-hanging-subject-pronoun? token)
       (make-turn-analysis (hanging-subject) false token "Trailing subject pronoun without predicate" 3000))
      ((is-dangling-aux-verb? token)
       (make-turn-analysis (dangling-aux) false token "Trailing auxiliary verb" 3000))
      (:else
       (make-turn-analysis (complete) true token "Utterance is grammatically complete" 0)))))
