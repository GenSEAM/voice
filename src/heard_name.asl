(module asl-voice/heard-name
  :d "Russian-to-Latin Cyrillic transliteration with longest digraph first and fuzzy Levenshtein distance"
  :x [NameMatch
      transliterate-cyrillic
      levenshtein-distance
      match-heard-name]
  :i [])

(dfs NameMatch
  (:f raw-input Str "raw phonetic input string")
  (:f transliterated Str "Latin-transliterated representation")
  (:f matched-target Str "closest matching candidate project name")
  (:f levenshtein-distance Int64 "computed edit distance")
  (:f score Float "normalized similarity score")
  (:f is-match Bool "true if edit distance is within configured tolerance"))

(dfs CandidateBest
  (:f target Str "best target candidate string")
  (:f dist Int64 "minimum edit distance found"))

(df transliterate-cyrillic [(text Str)] -> Str
  :d "Transliterates Cyrillic text to Latin using greedy longest-digraph substitution"
  (let [(t0 (string-lower text))
        (t1 (string-replace t0 "щ" "sch"))
        (t2 (string-replace t1 "ч" "ch"))
        (t3 (string-replace t2 "ш" "sh"))
        (t4 (string-replace t3 "ж" "zh"))
        (t5 (string-replace t4 "ю" "yu"))
        (t6 (string-replace t5 "я" "ya"))
        (t7 (string-replace t6 "э" "eh"))
        (t8 (string-replace t7 "ц" "ts"))
        (t9 (string-replace t8 "х" "kh"))
        (t10 (string-replace t9 "ё" "yo"))
        (t11 (string-replace t10 "а" "a"))
        (t12 (string-replace t11 "б" "b"))
        (t13 (string-replace t12 "в" "v"))
        (t14 (string-replace t13 "г" "g"))
        (t15 (string-replace t14 "д" "d"))
        (t16 (string-replace t15 "е" "e"))
        (t17 (string-replace t16 "з" "z"))
        (t18 (string-replace t17 "и" "i"))
        (t19 (string-replace t18 "й" "y"))
        (t20 (string-replace t19 "к" "k"))
        (t21 (string-replace t20 "л" "l"))
        (t22 (string-replace t21 "м" "m"))
        (t23 (string-replace t22 "н" "n"))
        (t24 (string-replace t23 "о" "o"))
        (t25 (string-replace t24 "п" "p"))
        (t26 (string-replace t25 "р" "r"))
        (t27 (string-replace t26 "с" "s"))
        (t28 (string-replace t27 "т" "t"))
        (t29 (string-replace t28 "у" "u"))
        (t30 (string-replace t29 "ф" "f"))
        (t31 (string-replace t30 "ъ" ""))
        (t32 (string-replace t31 "ы" "y"))
        (t33 (string-replace t32 "ь" ""))]
    t33))

(df make-initial-row [(target-len Int64) (curr Int64)] -> (List Int64)
  :d "Builds initial 0..len row for DP table"
  (if (> curr target-len)
    (list)
    (list-cons curr (make-initial-row target-len (+ curr 1)))))

(df compute-dp-cell [(c1 Str) (c2-list (List Str)) (prev-left Int64) (prev-tail (List Int64)) (curr-left Int64)] -> (List Int64)
  :d "Computes single DP row given previous row values and current row left neighbor"
  (if (list-empty? c2-list)
    (list)
    (let [(c2 (option-or (list-head c2-list) ""))
          (c2-tail (option-or (list-tail c2-list) (list)))
          (prev-above (option-or (list-head prev-tail) 0))
          (next-prev-tail (option-or (list-tail prev-tail) (list)))
          (cost (if (= c1 c2) 0 1))
          (ins (+ curr-left 1))
          (del (+ prev-above 1))
          (sub (+ prev-left cost))
          (cell (min ins (min del sub)))]
      (list-cons cell (compute-dp-cell c1 c2-tail prev-above next-prev-tail cell)))))

(df compute-dp-row [(c1 Str) (s2-chars (List Str)) (prev-row (List Int64)) (row-idx Int64)] -> (List Int64)
  :d "Computes new DP row starting from row index"
  (let [(p0 (option-or (list-head prev-row) 0))
        (p-tail (option-or (list-tail prev-row) (list)))]
    (list-cons row-idx (compute-dp-cell c1 s2-chars p0 p-tail row-idx))))

(df run-levenshtein-dp [(s1-chars (List Str)) (s2-chars (List Str)) (prev-row (List Int64)) (row-idx Int64)] -> (List Int64)
  :d "Folds characters of s1 producing final DP row"
  (if (list-empty? s1-chars)
    prev-row
    (let [(c1 (option-or (list-head s1-chars) ""))
          (s1-tail (option-or (list-tail s1-chars) (list)))
          (next-row (compute-dp-row c1 s2-chars prev-row row-idx))]
      (run-levenshtein-dp s1-tail s2-chars next-row (+ row-idx 1)))))

(df get-last-element [(lst (List Int64))] -> Int64
  :d "Returns the last element of a non-empty list of integers"
  (if (list-empty? lst)
    0
    (let [(t (option-or (list-tail lst) (list)))]
      (if (list-empty? t)
        (option-or (list-head lst) 0)
        (get-last-element t)))))

(df levenshtein-distance [(s1 Str) (s2 Str)] -> Int64
  :d "Computes standard character-level edit distance between two strings"
  (let [(c1 (string-chars s1))
        (c2 (string-chars s2))
        (len2 (list-length c2))
        (r0 (make-initial-row len2 0))
        (final-row (run-levenshtein-dp c1 c2 r0 1))]
    (get-last-element final-row)))

(df find-best-match [(trans Str) (candidates (List Str)) (curr-best CandidateBest)] -> CandidateBest
  :d "Recursively finds candidate with minimum edit distance"
  (if (list-empty? candidates)
    curr-best
    (let [(cand (option-or (list-head candidates) ""))
          (tail-cand (option-or (list-tail candidates) (list)))
          (d (levenshtein-distance trans cand))]
      (if (< d (.-dist curr-best))
        (find-best-match trans tail-cand (CandidateBest :target cand :dist d))
        (find-best-match trans tail-cand curr-best)))))

(df match-heard-name [(raw-heard Str) (candidates (List Str)) (max-distance Int64)] -> NameMatch
  :d "Transliterates raw input and matches against candidate list within tolerance"
  (let [(trans (transliterate-cyrillic raw-heard))
        (init (CandidateBest :target "" :dist 999999))
        (best (find-best-match trans candidates init))
        (b-dist (.-dist best))
        (b-target (.-target best))
        (len-t (string-length trans))
        (len-c (string-length b-target))
        (max-len (if (> len-t len-c) len-t len-c))
        (is-matched (and (> (string-length b-target) 0) (<= b-dist max-distance)))
        (score (if (<= max-len 0)
                 1.0
                 (- 1.0 (/ (int64-to-float64 b-dist) (int64-to-float64 max-len)))))]
    (NameMatch
      :raw-input raw-heard
      :transliterated trans
      :matched-target b-target
      :levenshtein-distance b-dist
      :score score
      :is-match is-matched)))
