import { PitchDetector } from "pitchy";

export const start = (onSample) => (onError) => () => {
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
        onSample({ frequency, clarity })();
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
