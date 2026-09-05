/**
 * Audio Ring Buffer & Low-Latency Voice Activity Detection (VAD) Bridge
 * Zero-dependency streaming audio ring buffer with barge-in interruption (<5ms).
 */

export class AudioRingBuffer {
  constructor(capacitySamples = 16000 * 5) { // 5 seconds at 16kHz
    this.buffer = new Float32Array(capacitySamples);
    this.capacity = capacitySamples;
    this.writeIndex = 0;
    this.totalWritten = 0;
  }

  write(samples) {
    const len = samples.length;
    for (let i = 0; i < len; i++) {
      this.buffer[(this.writeIndex + i) % this.capacity] = samples[i];
    }
    this.writeIndex = (this.writeIndex + len) % this.capacity;
    this.totalWritten += len;
  }

  readLatest(numSamples) {
    const count = Math.min(numSamples, this.totalWritten, this.capacity);
    const result = new Float32Array(count);
    const start = (this.writeIndex - count + this.capacity) % this.capacity;
    for (let i = 0; i < count; i++) {
      result[i] = this.buffer[(start + i) % this.capacity];
    }
    return result;
  }

  clear() {
    this.buffer.fill(0);
    this.writeIndex = 0;
    this.totalWritten = 0;
  }
}

export class VadEngine {
  constructor(options = {}) {
    this.sampleRate = options.sampleRate || 16000;
    this.frameSize = options.frameSize || 160; // 10ms at 16kHz
    this.energyThreshold = options.energyThreshold || 0.015;
    this.hangoverFrames = options.hangoverFrames || 5;

    this.isSpeech = false;
    this.consecutiveSpeech = 0;
    this.consecutiveSilence = 0;
    this.agentIsSpeaking = false;
    this.onBargeIn = options.onBargeIn || null;
  }

  /**
   * Root Mean Square (RMS) energy calculation
   */
  computeRms(frame) {
    let sum = 0;
    const len = frame.length;
    for (let i = 0; i < len; i++) {
      sum += frame[i] * frame[i];
    }
    return Math.sqrt(sum / len);
  }

  /**
   * Zero-crossing rate
   */
  computeZcr(frame) {
    let crossings = 0;
    for (let i = 1; i < frame.length; i++) {
      if ((frame[i] >= 0 && frame[i - 1] < 0) || (frame[i] < 0 && frame[i - 1] >= 0)) {
        crossings++;
      }
    }
    return crossings / frame.length;
  }

  /**
   * Process a single audio frame and update VAD state
   */
  processFrame(frame) {
    const t0 = performance.now();
    const rms = this.computeRms(frame);
    const rawSpeech = rms > this.energyThreshold;

    if (rawSpeech) {
      this.consecutiveSpeech++;
      this.consecutiveSilence = 0;
      this.isSpeech = true;

      // Conversational Barge-In Interrupt trigger
      if (this.agentIsSpeaking && this.consecutiveSpeech >= 2) {
        const dt = performance.now() - t0;
        if (this.onBargeIn) {
          this.onBargeIn({
            timestamp: Date.now(),
            latencyMs: +dt.toFixed(3),
            rms
          });
        }
        this.agentIsSpeaking = false; // Interrupted
      }
    } else {
      this.consecutiveSilence++;
      if (this.isSpeech && this.consecutiveSilence > this.hangoverFrames) {
        this.isSpeech = false;
        this.consecutiveSpeech = 0;
      }
    }

    return {
      isSpeech: this.isSpeech,
      rms,
      consecutiveSpeech: this.consecutiveSpeech,
      consecutiveSilence: this.consecutiveSilence
    };
  }

  setAgentSpeaking(speaking) {
    this.agentIsSpeaking = speaking;
  }
}
