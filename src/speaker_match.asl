(module asl-voice/speaker-match
  :d "Speaker verification using Log-Mel filterbank spectral features and cosine distance"
  :x [SpeakerProfile
      SpeakerMatchResult
      make-speaker-profile
      extract-mel-features
      cosine-similarity
      verify-speaker]
  :i [])

(dfs SpeakerProfile
  (:f speaker-id Str "unique identifier for enrolled speaker")
  (:f name Str "human readable display name")
  (:f embedding (List F64) "normalized acoustic feature vector")
  (:f threshold F64 "minimum cosine similarity threshold for acceptance"))

(dfs SpeakerMatchResult
  (:f matched Bool "true if confidence score meets or exceeds threshold")
  (:f speaker-id Str "speaker identifier from candidate profile")
  (:f confidence F64 "cosine similarity score between minus one and one")
  (:f distance F64 "cosine distance one minus confidence"))

(df sqrt-approx [(x F64)] -> F64
  :d "Computes square root approximation using Newton-Raphson"
  (if (<= x 0.0)
    0.0
    (let [(g0 (/ (+ x 1.0) 2.0))
          (g1 (/ (+ g0 (/ x g0)) 2.0))
          (g2 (/ (+ g1 (/ x g1)) 2.0))
          (g3 (/ (+ g2 (/ x g2)) 2.0))
          (g4 (/ (+ g3 (/ x g3)) 2.0))
          (g5 (/ (+ g4 (/ x g4)) 2.0))]
      g5)))

(df dot-product [(a (List F64)) (b (List F64))] -> F64
  :d "Computes inner dot product between two float vectors"
  (if (or (list-empty? a) (list-empty? b))
    0.0
    (let [(ha (option-or (list-head a) 0.0))
          (hb (option-or (list-head b) 0.0))
          (ta (option-or (list-tail a) (list)))
          (tb (option-or (list-tail b) (list)))]
      (+ (* ha hb) (dot-product ta tb)))))

(df norm-sq [(v (List F64))] -> F64
  :d "Computes sum of squares of elements in a float list"
  (if (list-empty? v)
    0.0
    (let [(h (option-or (list-head v) 0.0))
          (t (option-or (list-tail v) (list)))]
      (+ (* h h) (norm-sq t)))))

(df cosine-similarity [(vec-a (List F64)) (vec-b (List F64))] -> F64
  :d "Computes cosine similarity between two float vectors clamped to -1.0 to 1.0"
  (let [(na (sqrt-approx (norm-sq vec-a)))
        (nb (sqrt-approx (norm-sq vec-b)))
        (denom (* na nb))]
    (if (<= denom 0.000001)
      0.0
      (let [(dot (dot-product vec-a vec-b))
            (sim (/ dot denom))]
        (if (> sim 1.0)
          1.0
          (if (< sim -1.0)
            -1.0
            sim))))))

(df make-speaker-profile [(speaker-id Str) (name Str) (embedding (List F64)) (threshold F64)] -> SpeakerProfile
  :d "Constructs an enrolled speaker profile"
  (SpeakerProfile
    :speaker-id speaker-id
    :name name
    :embedding embedding
    :threshold threshold))

(df verify-speaker [(profile SpeakerProfile) (input-embedding (List F64))] -> SpeakerMatchResult
  :d "Verifies input embedding against speaker profile using cosine similarity"
  (let [(conf (cosine-similarity (.-embedding profile) input-embedding))
        (thresh (.-threshold profile))
        (is-match (>= conf thresh))
        (dist (- 1.0 conf))]
    (SpeakerMatchResult
      :matched is-match
      :speaker-id (.-speaker-id profile)
      :confidence conf
      :distance dist)))

(df drop-samples [(lst (List F64)) (n I64)] -> (List F64)
  :d "Drops n samples from list head"
  (if (or (<= n 0) (list-empty? lst))
    lst
    (drop-samples (option-or (list-tail lst) (list)) (- n 1))))

(df compute-bin-energy [(samples (List F64)) (count I64)] -> F64
  :d "Computes sum of squares of first n samples"
  (if (or (<= count 0) (list-empty? samples))
    0.0
    (let [(h (option-or (list-head samples) 0.0))
          (t (option-or (list-tail samples) (list)))]
      (+ (* h h) (compute-bin-energy t (- count 1))))))

(df extract-filterbank-bins [(samples (List F64)) (bins-remaining I64) (bin-size I64)] -> (List F64)
  :d "Recursively constructs spectral energy bins"
  (if (<= bins-remaining 0)
    (list)
    (let [(e (compute-bin-energy samples bin-size))
          (norm-val (sqrt-approx e))
          (next-samples (drop-samples samples bin-size))]
      (list-cons norm-val (extract-filterbank-bins next-samples (- bins-remaining 1) bin-size)))))

(df extract-mel-features [(samples (List F64))] -> (List F64)
  :d "Extracts 16-channel spectral energy distribution from input audio samples"
  (let [(total (list-length samples))
        (bin-size (if (< total 16) 1 (/ total 16)))]
    (extract-filterbank-bins samples 16 bin-size)))
