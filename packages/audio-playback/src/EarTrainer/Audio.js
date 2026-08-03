import * as Tone from "tone";

const sampleUrls = {
  A0: "A0v8.mp3",
  A1: "A1v8.mp3",
  A2: "A2v8.mp3",
  A3: "A3v8.mp3",
  A4: "A4v8.mp3",
  A5: "A5v8.mp3",
  A6: "A6v8.mp3",
  A7: "A7v8.mp3",
  C1: "C1v8.mp3",
  C2: "C2v8.mp3",
  C3: "C3v8.mp3",
  C4: "C4v8.mp3",
  C5: "C5v8.mp3",
  C6: "C6v8.mp3",
  C7: "C7v8.mp3",
  C8: "C8v8.mp3",
  "D#1": "Ds1v8.mp3",
  "D#2": "Ds2v8.mp3",
  "D#3": "Ds3v8.mp3",
  "D#4": "Ds4v8.mp3",
  "D#5": "Ds5v8.mp3",
  "D#6": "Ds6v8.mp3",
  "D#7": "Ds7v8.mp3",
  "F#1": "Fs1v8.mp3",
  "F#2": "Fs2v8.mp3",
  "F#3": "Fs3v8.mp3",
  "F#4": "Fs4v8.mp3",
  "F#5": "Fs5v8.mp3",
  "F#6": "Fs6v8.mp3",
  "F#7": "Fs7v8.mp3",
};

const sampleBaseUrl = new URL("./audio/salamander/", document.baseURI).href;
const midiNote = (midi) => Tone.Frequency(midi, "midi").toNote();
const samplerReady = new WeakMap();
const playbackState = new WeakMap();
let contextConfigured = false;

const beginPlayback = (sampler) => {
  const previous = playbackState.get(sampler) ?? { generation: 0, timers: [] };
  previous.timers.forEach(clearTimeout);
  const generation = previous.generation + 1;
  playbackState.set(sampler, { generation, timers: [] });
  sampler.releaseAll();
  return generation;
};

const isCurrentPlayback = (sampler, generation) =>
  playbackState.get(sampler)?.generation === generation;

const schedulePlayback = (sampler, generation, delay, action) => {
  const timer = setTimeout(() => {
    if (isCurrentPlayback(sampler, generation)) action();
  }, delay);
  playbackState.get(sampler)?.timers.push(timer);
};

const triggerNote = (sampler, generation, note, duration, time = Tone.now()) => {
  sampler.triggerAttack(note, time);
  schedulePlayback(sampler, generation, duration * 1000, () =>
    sampler.triggerRelease(note),
  );
};

export const createSampler = () => {
  if (!contextConfigured) {
    Tone.setContext(new Tone.Context({ latencyHint: "balanced" }), true);
    contextConfigured = true;
  }

  let markReady;
  let markFailed;
  const ready = new Promise((resolve, reject) => {
    markReady = resolve;
    markFailed = reject;
  });
  void ready.catch(() => {});
  const sampler = new Tone.Sampler({
    urls: sampleUrls,
    baseUrl: sampleBaseUrl,
    release: 0.7,
    onload: markReady,
    onerror: (error) =>
      markFailed(
        error instanceof Error
          ? error
          : new Error("Piano samples could not be loaded."),
      ),
  }).toDestination();
  samplerReady.set(sampler, ready);
  playbackState.set(sampler, { generation: 0, timers: [] });
  return sampler;
};

export const playIntervalImpl = (sampler) => (rootMidi) => (targetMidi) => (mode) => (onStarted) => (onError) => () => {
  const generation = beginPlayback(sampler);
  void (async () => {
    try {
      await Tone.start();
      await samplerReady.get(sampler);
      if (!isCurrentPlayback(sampler, generation)) return;

      const root = midiNote(rootMidi);
      const target = midiNote(targetMidi);
      const now = Tone.now() + 0.05;

      if (mode === "harmonic") {
        triggerNote(sampler, generation, [root, target], 0.9, now);
      } else {
        triggerNote(sampler, generation, root, 0.65, now);
        schedulePlayback(sampler, generation, 800, () =>
          triggerNote(sampler, generation, target, 0.65),
        );
      }
      onStarted();
    } catch (error) {
      if (!isCurrentPlayback(sampler, generation)) return;
      const message = error instanceof Error ? error.message : String(error);
      onError(message)();
    }
  })();
};

export const playRootImpl = (sampler) => (rootMidi) => (onStarted) => (onError) => () => {
  const generation = beginPlayback(sampler);
  void (async () => {
    try {
      await Tone.start();
      await samplerReady.get(sampler);
      if (!isCurrentPlayback(sampler, generation)) return;

      triggerNote(sampler, generation, midiNote(rootMidi), 0.9, Tone.now() + 0.05);
      onStarted();
    } catch (error) {
      if (!isCurrentPlayback(sampler, generation)) return;
      const message = error instanceof Error ? error.message : String(error);
      onError(message)();
    }
  })();
};

export const stop = (sampler) => () => {
  const previous = playbackState.get(sampler) ?? { generation: 0, timers: [] };
  previous.timers.forEach(clearTimeout);
  playbackState.set(sampler, { generation: previous.generation + 1, timers: [] });
  sampler.releaseAll();
};
