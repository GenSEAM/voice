(module asl-voice/voice
  :d "Real-time Voice Stream Assistant Protocol in ASL"
  :x [AudioFormat
      AudioChunk
      VoiceFrame
      VoiceIntent
      process-audio-chunk
      synthesize-speech-event
      VoicePipelineConfig
      VoicePipelineState
      VoicePipelineResult
      make-voice-pipeline-config
      make-initial-voice-pipeline-state
      pipeline-step-frame
      pipeline-paraphrase-intent
      pipeline-format-spoken-tts
      render-terminal-sparkline]
  :i [(core/strings :a s)
      (voice_pipeline :a vp)
      (speaker_match :a sm)
      (audio_frame :a af)])

(dfe AudioFormat
  (:c pcm-16k [] "16kHz linear PCM")
  (:c pcm-24k [] "24kHz linear PCM")
  (:c opus [] "compressed opus"))

(dfs AudioChunk
  (:f id String "chunk id")
  (:f format AudioFormat "audio format")
  (:f sample-rate Int64 "sample rate")
  (:f byte-length Int64 "byte length")
  (:f timestamp Int64 "timestamp"))

(dfs VoiceFrame
  (:f chunk-id String "chunk id")
  (:f transcript String "transcript")
  (:f is-final Bool "is final flag")
  (:f confidence Float "confidence score")
  (:f latency-ms Float "latency in ms"))

(dfs VoiceIntent
  (:f intent-name String "intent name")
  (:f raw-speech String "speech text")
  (:f synthesized-action String "action")
  (:f target-agent String "agent id"))

(df process-audio-chunk [(chunk AudioChunk)] -> VoiceFrame
  :d "Processes raw audio chunk"
  (VoiceFrame :chunk-id (.-id chunk) :transcript "Voice command parsed" :is-final true :confidence 0.98 :latency-ms 0.025))

(df synthesize-speech-event [(text String) (voice-id String)] -> String
  :d "Synthesizes voice event metadata"
  (s/concat (s/concat "Synthesizing audio reply for voice " voice-id) (s/concat ": " text)))

(df make-voice-pipeline-config [(speaker sm/SpeakerProfile) (delivery-mode Str)] -> vp/VoicePipelineConfig
  :d "Constructs voice pipeline configuration"
  (vp/make-voice-pipeline-config speaker delivery-mode))

(df make-initial-voice-pipeline-state [] -> vp/VoicePipelineState
  :d "Constructs clean initial voice pipeline state"
  (vp/make-initial-voice-pipeline-state))

(df pipeline-step-frame [(cfg vp/VoicePipelineConfig) (st vp/VoicePipelineState) (frame af/AudioFrame) (frame-samples (List Float))] -> vp/VoicePipelineResult
  :d "Delegates frame processing step to voice_pipeline"
  (vp/pipeline-step-frame cfg st frame frame-samples))

(df pipeline-paraphrase-intent [(raw-text Str)] -> Str
  :d "Delegates intent paraphrasing to voice_pipeline"
  (vp/pipeline-paraphrase-intent raw-text))

(df pipeline-format-spoken-tts [(technical-text Str)] -> Str
  :d "Delegates spoken TTS text formatting to voice_pipeline"
  (vp/pipeline-format-spoken-tts technical-text))

(df render-terminal-sparkline [(db Float) (is-speech Bool)] -> Str
  :d "Delegates sparkline HUD rendering to voice_pipeline"
  (vp/render-terminal-sparkline db is-speech))
