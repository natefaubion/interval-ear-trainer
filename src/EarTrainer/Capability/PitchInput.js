import { PitchDetector } from "pitchy";

// The analyser exposes its time-domain window as a mutable Float32Array.

const decibels = (buffer) => {
  let sum = 0;
  for (const value of buffer) sum += value * value;
  const rms = Math.sqrt(sum / buffer.length);
  return 20 * Math.log10(Math.max(rms, 1e-8));
};

const stopMonitor = (monitor) => {
  monitor.stopped = true;
  cancelAnimationFrame(monitor.animationFrame);
  monitor.stream?.getTracks().forEach((track) => track.stop());
  if (monitor.audioContext && monitor.audioContext.state !== "closed") {
    void monitor.audioContext.close();
  }
};

export const startImpl = (onSample) => (onError, onSuccess) => {
  const monitor = {
    animationFrame: 0,
    audioContext: null,
    stopped: false,
    stream: null,
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
        onSample({
          clarity,
          decibels: decibels(input),
          frequency,
          time: now,
        })();
        monitor.animationFrame = requestAnimationFrame(readPitch);
      };

      readPitch();
      onSuccess(monitor);
    } catch (error) {
      if (monitor.stopped) return;
      onError(error instanceof Error ? error : new Error(String(error)));
    }
  })();

  return (_error, _onError, onCancel) => {
    stopMonitor(monitor);
    onCancel();
  };
};

export const stop = (monitor) => () => stopMonitor(monitor);
