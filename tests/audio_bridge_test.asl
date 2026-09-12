(module asl-voice/test-audio-bridge
  :d "Unit verification test suite for Audio Bridge Ingestion, Ring Buffer Bounding, and Hyness Workstation Profile"
  :x [test-audio-bridge-initialization-and-ingestion
      test-ring-buffer-capacity-overflow-bounding
      test-quiet-edge-detection
      test-speech-window-draining
      test-hyness-profile-defaults-and-aliases
      test-hyness-endpoints-and-asn-rendering
      run-all-bridge-tests]
  :i [(audio_bridge :a ab)
      (hyness_profile :a hp)])

(df test-audio-bridge-initialization-and-ingestion [] -> Bool
  (let [(cfg (ab/make-audio-bridge-config 16000 20 4 0.05))
        (st0 (ab/init-audio-bridge cfg))
        (c1 (ab/make-audio-chunk "c1" 320 0.08 0.05 1000))
        (c2 (ab/make-audio-chunk "c2" 320 0.09 0.05 1020))
        (st1 (ab/ingest-audio-chunk st0 c1))
        (st2 (ab/ingest-audio-chunk st1 c2))]
    (assert (= (.-total-ingested-chunks st2) 2) "two chunks ingested")
    (assert (= (.-consecutive-speech-chunks st2) 2) "consecutive speech run is 2")
    (assert (= (.-consecutive-silence-chunks st2) 0) "silence run is 0")
    (assert (= (list-length (.-ring-chunks st2)) 2) "ring buffer holds 2 chunks")
    true))

(df test-ring-buffer-capacity-overflow-bounding [] -> Bool
  (let [(cfg (ab/make-audio-bridge-config 16000 20 4 0.05))
        (st0 (ab/init-audio-bridge cfg))
        (c1 (ab/make-audio-chunk "c1" 320 0.08 0.05 1000))
        (c2 (ab/make-audio-chunk "c2" 320 0.08 0.05 1020))
        (c3 (ab/make-audio-chunk "c3" 320 0.08 0.05 1040))
        (c4 (ab/make-audio-chunk "c4" 320 0.08 0.05 1060))
        (c5 (ab/make-audio-chunk "c5" 320 0.08 0.05 1080))
        (st (ab/ingest-audio-chunk (ab/ingest-audio-chunk (ab/ingest-audio-chunk (ab/ingest-audio-chunk (ab/ingest-audio-chunk st0 c1) c2) c3) c4) c5))]
    (assert (= (.-total-ingested-chunks st) 5) "five total chunks ingested")
    (assert (= (list-length (.-ring-chunks st)) 4) "ring buffer clamped at capacity 4")
    (let [(oldest (list-first (.-ring-chunks st)))]
      (assert (= (.-chunk-id oldest) "c2") "oldest chunk c1 evicted in FIFO order"))
    true))

(df test-quiet-edge-detection [] -> Bool
  (let [(cfg (ab/make-audio-bridge-config 16000 20 10 0.05))
        (st0 (ab/init-audio-bridge cfg))
        (c-sp1 (ab/make-audio-chunk "s1" 320 0.08 0.05 1000))
        (c-sp2 (ab/make-audio-chunk "s2" 320 0.09 0.05 1020))
        (c-sil1 (ab/make-audio-chunk "q1" 320 0.01 0.05 1040))
        (c-sil2 (ab/make-audio-chunk "q2" 320 0.01 0.05 1060))
        (c-sil3 (ab/make-audio-chunk "q3" 320 0.01 0.05 1080))
        (st (ab/ingest-audio-chunk (ab/ingest-audio-chunk (ab/ingest-audio-chunk (ab/ingest-audio-chunk (ab/ingest-audio-chunk st0 c-sp1) c-sp2) c-sil1) c-sil2) c-sil3))]
    (assert (= (.-consecutive-silence-chunks st) 3) "consecutive silence run is 3")
    (assert (ab/is-quiet-edge-detected? st 3) "quiet edge detected at threshold 3")
    (refute (ab/is-quiet-edge-detected? st 5) "quiet edge not yet reached for threshold 5")
    true))

(df test-speech-window-draining [] -> Bool
  (let [(cfg (ab/make-audio-bridge-config 16000 20 6 0.05))
        (st0 (ab/init-audio-bridge cfg))
        (c1 (ab/make-audio-chunk "c1" 320 0.08 0.05 1000))
        (c2 (ab/make-audio-chunk "c2" 320 0.08 0.05 1020))
        (st-full (ab/ingest-audio-chunk (ab/ingest-audio-chunk st0 c1) c2))
        (drain-res (ab/drain-speech-window st-full))
        (st-drained (pair-first drain-res))
        (extracted (pair-second drain-res))]
    (assert (= (list-length extracted) 2) "extracted exactly 2 chunks")
    (assert (= (list-length (.-ring-chunks st-drained)) 0) "ring buffer cleared")
    (assert (= (.-consecutive-speech-chunks st-drained) 0) "speech counter reset")
    (assert (= (.-total-ingested-chunks st-drained) 2) "lifetime ingested chunks preserved")
    true))

(df test-hyness-profile-defaults-and-aliases [] -> Bool
  (let [(prof (hp/make-default-hyness-profile))]
    (assert (= (.-primary-locale prof) "en") "primary locale is en")
    (assert (= (.-secondary-locale prof) "ru") "secondary locale is ru")
    (assert (.-is-mixed-mode prof) "mixed mode enabled")
    (assert (= (hp/resolve-hyness-alias prof "асекс") "genseam") "resolves асекс to asex")
    (assert (= (hp/resolve-hyness-alias prof "хайнесс") "harness") "resolves хайнесс to harness")
    (assert (= (hp/resolve-hyness-alias prof "ханес") "harness") "resolves ханес to harness")
    (assert (= (hp/resolve-hyness-alias prof "шроуди") "shrody") "resolves шроуди to shrody")
    (assert (= (hp/resolve-hyness-alias prof "unknown") "unknown") "preserves unmapped word")
    (let [(normalized (hp/apply-hyness-aliases prof "запусти асекс в хайнесс и проверь шроуди"))]
      (assert (string-contains? normalized "genseam") "replaces асекс with asex")
      (assert (string-contains? normalized "harness") "replaces хайнесс with harness")
      (assert (string-contains? normalized "shrody") "replaces шроуди with shrody"))
    true))

(df test-hyness-endpoints-and-asn-rendering [] -> Bool
  (let [(prof (hp/make-default-hyness-profile))
        (cfg (ab/make-audio-bridge-config 16000 20 4 0.05))
        (st (ab/init-audio-bridge cfg))
        (cdp-ep (hp/find-endpoint-by-kind prof "cdp"))
        (hub-ep (hp/find-endpoint-by-kind prof "hub"))
        (none-ep (hp/find-endpoint-by-kind prof "nonexistent"))
        (prof-asn (hp/format-hyness-profile-asn prof))
        (bridge-asn (ab/render-bridge-telemetry-asn st))]
    (assert (string-contains? cdp-ep "ws://127.0.0.1:9222") "cdp endpoint resolved")
    (assert (string-contains? hub-ep "~/.asl/run/hub.sock") "hub endpoint resolved")
    (assert (= none-ep "") "empty string for unknown endpoint")
    (assert (string-contains? prof-asn ":name \"hyness-workstation\"") "profile ASN contains name")
    (assert (string-contains? prof-asn ":alias-count 6") "profile ASN reflects 6 aliases")
    (assert (string-contains? bridge-asn ":audio-bridge") "bridge telemetry contains tag")
    true))

(df run-all-bridge-tests [] -> Bool
  (do
    (test-audio-bridge-initialization-and-ingestion)
    (test-ring-buffer-capacity-overflow-bounding)
    (test-quiet-edge-detection)
    (test-speech-window-draining)
    (test-hyness-profile-defaults-and-aliases)
    (test-hyness-endpoints-and-asn-rendering)
    true))

(run-all-bridge-tests)
