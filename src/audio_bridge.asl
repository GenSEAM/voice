(module asl-voice/audio-bridge
  :d "Local Audio Bridge Daemon: Ingestion of streaming 16kHz PCM audio chunks, ring buffer bounding, and quiet edge detection"
  :x [AudioBridgeConfig
      BridgeAudioChunk
      AudioBridgeState
      make-audio-bridge-config
      make-audio-chunk
      init-audio-bridge
      ingest-audio-chunk
      is-quiet-edge-detected?
      drain-speech-window
      render-bridge-telemetry-asn]
  :i [(asl-text/string :a s)])

(dfs AudioBridgeConfig
  (:f sample-rate I64 "Audio sample rate in Hz e.g. 16000")
  (:f frame-duration-ms I64 "Duration of each PCM frame in milliseconds")
  (:f ring-capacity-chunks I64 "Maximum number of chunks retained in ring buffer")
  (:f speech-energy-threshold F64 "RMS energy threshold for speech activity"))

(dfs BridgeAudioChunk
  (:f chunk-id Str "Unique audio chunk identifier")
  (:f samples-count I64 "Number of samples in chunk")
  (:f energy F64 "RMS energy level")
  (:f is-speech Bool "True if chunk energy exceeds speech threshold")
  (:f timestamp-ms I64 "Monotonic millisecond timestamp"))

(dfs AudioBridgeState
  (:f config AudioBridgeConfig "Bridge operational parameters")
  (:f ring-chunks (List BridgeAudioChunk) "Retained circular buffer chunks")
  (:f is-recording Bool "Audio stream active flag")
  (:f total-ingested-chunks I64 "Lifetime ingested chunk counter")
  (:f consecutive-speech-chunks I64 "Counter of consecutive speech chunks")
  (:f consecutive-silence-chunks I64 "Counter of consecutive silence chunks"))

(df make-audio-bridge-config [(sample-rate I64) (frame-dur-ms I64) (cap I64) (threshold F64)] -> AudioBridgeConfig
  :d "Constructs AudioBridgeConfig record"
  (AudioBridgeConfig
    :sample-rate sample-rate
    :frame-duration-ms frame-dur-ms
    :ring-capacity-chunks cap
    :speech-energy-threshold threshold))

(df make-audio-chunk [(id Str) (samples I64) (energy F64) (threshold F64) (ts I64)] -> BridgeAudioChunk
  :d "Constructs BridgeAudioChunk with automated speech threshold classification"
  (BridgeAudioChunk
    :chunk-id id
    :samples-count samples
    :energy energy
    :is-speech (>= energy threshold)
    :timestamp-ms ts))

(df init-audio-bridge [(cfg AudioBridgeConfig)] -> AudioBridgeState
  :d "Initializes clean audio bridge state"
  (AudioBridgeState
    :config cfg
    :ring-chunks (list)
    :is-recording true
    :total-ingested-chunks 0
    :consecutive-speech-chunks 0
    :consecutive-silence-chunks 0))

(df ingest-audio-chunk [(bridge AudioBridgeState) (chunk BridgeAudioChunk)] -> AudioBridgeState
  :d "Ingests incoming PCM chunk into circular buffer with FIFO overflow capping and run tracking"
  (let [(cap (.-ring-capacity-chunks (.-config bridge)))
        (current (.-ring-chunks bridge))
        (appended (list-append current (list chunk)))
        (appended-len (list-length appended))
        (excess (- appended-len cap))
        (retained (if (> excess 0) (list-drop appended excess) appended))
        (is-sp (.-is-speech chunk))
        (new-sp-run (if is-sp (+ (.-consecutive-speech-chunks bridge) 1) 0))
        (new-sil-run (if is-sp 0 (+ (.-consecutive-silence-chunks bridge) 1)))
        (new-total (+ (.-total-ingested-chunks bridge) 1))]
    (AudioBridgeState
      :config (.-config bridge)
      :ring-chunks retained
      :is-recording (.-is-recording bridge)
      :total-ingested-chunks new-total
      :consecutive-speech-chunks new-sp-run
      :consecutive-silence-chunks new-sil-run)))

(df is-quiet-edge-detected? [(bridge AudioBridgeState) (silence-threshold-chunks I64)] -> Bool
  :d "Determines if silence run has exceeded quiet-edge threshold while speech was previously observed"
  (and (>= (.-consecutive-silence-chunks bridge) silence-threshold-chunks)
       (> (.-total-ingested-chunks bridge) (.-consecutive-silence-chunks bridge))))

(df drain-speech-window [(bridge AudioBridgeState)] -> (Pair AudioBridgeState (List BridgeAudioChunk))
  :d "Extracts buffered speech chunks and resets ring buffer"
  (let [(chunks (.-ring-chunks bridge))
        (cleared (AudioBridgeState
                   :config (.-config bridge)
                   :ring-chunks (list)
                   :is-recording (.-is-recording bridge)
                   :total-ingested-chunks (.-total-ingested-chunks bridge)
                   :consecutive-speech-chunks 0
                   :consecutive-silence-chunks 0))]
    (pair cleared chunks)))

(df render-bridge-telemetry-asn [(bridge AudioBridgeState)] -> Str
  :d "Renders high-density ASN telemetry summarizing active bridge state"
  (let [(c-len (list-length (.-ring-chunks bridge)))
        (tot (string-from-int64 (.-total-ingested-chunks bridge)))
        (sp (string-from-int64 (.-consecutive-speech-chunks bridge)))
        (sil (string-from-int64 (.-consecutive-silence-chunks bridge)))]
    (str "(:audio-bridge :buffered-chunks " (string-from-int64 c-len) " :total-ingested " tot " :speech-run " sp " :silence-run " sil ")")))
