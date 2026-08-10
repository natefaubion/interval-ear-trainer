import { PitchDetector } from "pitchy";

// The analyser exposes its time-domain window as a mutable Float32Array.

const rootMeanSquare = (buffer) => {
  let sum = 0;
  for (const value of buffer) sum += value * value;
  return Math.sqrt(sum / buffer.length);
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
      const windowSizes = [2048, 4096, 8192];
      const analyser = audioContext.createAnalyser();
      analyser.fftSize = 8192;
      audioContext.createMediaStreamSource(stream).connect(analyser);

      const input = new Float32Array(analyser.fftSize);
      const analyses = windowSizes.map((windowSize) => ({
        detector: PitchDetector.forFloat32Array(windowSize),
        input: new Float32Array(windowSize),
        windowSize,
      }));
      monitor.audioContext = audioContext;
      monitor.stream = stream;

      const readPitch = () => {
        if (monitor.stopped) return;
        analyser.getFloatTimeDomainData(input);
        const candidates = analyses.map((analysis) => {
          analysis.input.set(input.subarray(input.length - analysis.windowSize));
          const [frequency, clarity] = analysis.detector.findPitch(
            analysis.input,
            audioContext.sampleRate,
          );
          return { clarity, frequency, windowSize: analysis.windowSize };
        });
        const now = performance.now();
        onSample({
          candidates,
          rms: rootMeanSquare(analyses[0].input),
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
