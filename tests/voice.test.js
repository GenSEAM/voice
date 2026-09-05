import test from "node:test";
import assert from "node:assert/strict";
import { VoiceStreamBridge } from "../bridges/ts/index.ts";
import { AudioRingBuffer, VadEngine } from "../bridges/vad.js";

test("VoiceStreamBridge - Linear PCM Downsampling & Transcript Intent", () => {
  const bridge = new VoiceStreamBridge({ sampleRate: 16000 });
  assert.equal(bridge["config"].sampleRate, 16000);

  // Generate 48kHz audio buffer
  const input48k = new Float32Array(4800);
  for (let i = 0; i < input48k.length; i++) {
    input48k[i] = Math.sin((2 * Math.PI * 440 * i) / 48000);
  }

  // Downsample to 16kHz
  const pcm16 = VoiceStreamBridge.downsampleTo16k(input48k, 48000);
  assert.equal(pcm16.length, 1600); // exactly 1/3 length
  assert.ok(pcm16 instanceof Int16Array);

  // Process transcript to intent
  const intent = bridge.processTranscript("analyze codebase architecture", "eddie-frontline");
  assert.equal(intent.intent, "agent-voice-instruction");
  assert.equal(intent.targetAgent, "eddie-frontline");
  assert.ok(intent.action.includes("eddie-frontline"));
  assert.ok(intent.latencyMs < 1.0);
});

test("AudioRingBuffer - Circular FIFO & Overwrite Protection", () => {
  const ring = new AudioRingBuffer(100);

  const chunk1 = new Float32Array(60).fill(1.0);
  ring.write(chunk1);
  assert.equal(ring.totalWritten, 60);

  const chunk2 = new Float32Array(60).fill(2.0);
  ring.write(chunk2);
  assert.equal(ring.totalWritten, 120);

  // Buffer holds last 100 samples
  const latest50 = ring.readLatest(50);
  assert.equal(latest50.length, 50);
  assert.equal(latest50[0], 2.0);
  assert.equal(latest50[49], 2.0);

  ring.clear();
  assert.equal(ring.totalWritten, 0);
});

test("VadEngine - Speech Detection & Conversational Barge-In Latency", () => {
  let bargeInFired = null;
  const vad = new VadEngine({
    energyThreshold: 0.02,
    hangoverFrames: 3,
    onBargeIn: (ev) => {
      bargeInFired = ev;
    }
  });

  vad.setAgentSpeaking(true);

  // Send silence frame (RMS near 0)
  const silence = new Float32Array(160).fill(0.001);
  const st1 = vad.processFrame(silence);
  assert.equal(st1.isSpeech, false);
  assert.equal(bargeInFired, null);

  // Send speech frame 1 (RMS ~ 0.5)
  const speech1 = new Float32Array(160).fill(0.5);
  const st2 = vad.processFrame(speech1);
  assert.equal(st2.isSpeech, true);

  // Send speech frame 2 -> triggers barge-in cutoff
  const st3 = vad.processFrame(speech1);
  assert.equal(st3.isSpeech, true);
  assert.ok(bargeInFired !== null, "Barge-in interrupt event should have fired");
  assert.ok(bargeInFired.latencyMs < 5.0, `Barge-in latency ${bargeInFired.latencyMs}ms must be < 5ms`);
  assert.equal(vad.agentIsSpeaking, false);
});
