import { PitchDetector } from "pitchy";

const clarityThreshold = 0.9;
const maximumFrequency = 1200;
const minimumFrequency = 70;
const minimumSamples = 4;
const sampleWindowMilliseconds = 300;
const silenceMilliseconds = 180;
const volumeThresholdDb = -50;

const decibels = (buffer) => {
  let sum = 0;
  for (const value of buffer) sum += value * value;
  const rms = Math.sqrt(sum / buffer.length);
  return 20 * Math.log10(Math.max(rms, 1e-8));
};

const median = (values) => {
  const sorted = [...values].sort((left, right) => left - right);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1
    ? sorted[middle]
    : (sorted[middle - 1] + sorted[middle]) / 2;
};

export const start = (onSample) => (onError) => () => {
  const monitor = {
    animationFrame: 0,
    audioContext: null,
    stopped: false,
    stream: null,
    samples: [],
    lastValidAt: 0,
  };

  void (async () => {
    try {
      const stream = await navigator.mediaDevices.getUserMedia({
        audio: {
          autoGainControl: false,
          echoCancellation: false,
          noiseSuppression: false,
        },
      });

      if (monitor.stopped) {
        stream.getTracks().forEach((track) => track.stop());
        return;
      }

      const audioContext = new AudioContext();
      const analyser = audioContext.createAnalyser();
      analyser.fftSize = 2048;
      audioContext.createMediaStreamSource(stream).connect(analyser);

      const input = new Float32Array(analyser.fftSize);
      const detector = PitchDetector.forFloat32Array(analyser.fftSize);
      monitor.audioContext = audioContext;
      monitor.stream = stream;

      const readPitch = () => {
        if (monitor.stopped) return;
        analyser.getFloatTimeDomainData(input);
        const [frequency, clarity] = detector.findPitch(input, audioContext.sampleRate);
        const now = performance.now();
        const valid =
          clarity >= clarityThreshold &&
          decibels(input) >= volumeThresholdDb &&
          frequency >= minimumFrequency &&
          frequency <= maximumFrequency;

        if (valid) {
          monitor.samples.push({ frequency, clarity, time: now });
          monitor.lastValidAt = now;
        }
        monitor.samples = monitor.samples.filter(
          (sample) => now - sample.time <= sampleWindowMilliseconds,
        );

        if (monitor.samples.length >= minimumSamples) {
          onSample({
            clarity: median(monitor.samples.map((sample) => sample.clarity)),
            frequency: median(monitor.samples.map((sample) => sample.frequency)),
          })();
        } else if (now - monitor.lastValidAt >= silenceMilliseconds) {
          onSample({ frequency: 0, clarity: 0 })();
        }
        monitor.animationFrame = requestAnimationFrame(readPitch);
      };

      readPitch();
    } catch (error) {
      if (monitor.stopped) return;
      const message = error instanceof Error ? error.message : String(error);
      onError(message)();
    }
  })();

  return monitor;
};

export const stop = (monitor) => () => {
  monitor.stopped = true;
  cancelAnimationFrame(monitor.animationFrame);
  monitor.stream?.getTracks().forEach((track) => track.stop());
  if (monitor.audioContext && monitor.audioContext.state !== "closed") {
    void monitor.audioContext.close();
  }
};
