import * as Tone from "tone";

// Tone owns the sampler and audio clock; PureScript supplies the playback plan.

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
const samplerReady = new WeakMap();
const playbackState = new WeakMap();
let contextConfigured = false;

const cancelPlayback = (sampler) => {
  const state = playbackState.get(sampler);
  if (state) state.cancelled = true;
  state?.timers.forEach(clearTimeout);
  playbackState.delete(sampler);
  sampler.releaseAll();
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
  return sampler;
};

export const playImpl = (sampler) => (events) => (durationMilliseconds) => (onError, onSuccess) => {
  cancelPlayback(sampler);
  const state = { cancelled: false, timers: [] };
  playbackState.set(sampler, state);

  void (async () => {
    try {
      await Tone.start();
      await samplerReady.get(sampler);
      if (state.cancelled) return;

      for (const event of events) {
        const timer = setTimeout(() => {
          if (state.cancelled) return;
          const notes = event.notes.map((midi) => Tone.Frequency(midi, "midi").toNote());
          sampler.triggerAttackRelease(notes, event.durationMilliseconds / 1000);
        }, event.startMilliseconds);
        state.timers.push(timer);
      }
      state.timers.push(setTimeout(() => {
        if (state.cancelled) return;
        playbackState.delete(sampler);
        onSuccess();
      }, durationMilliseconds));
    } catch (error) {
      if (!state.cancelled) {
        cancelPlayback(sampler);
        onError(error instanceof Error ? error : new Error(String(error)));
      }
    }
  })();

  return (_error, _onError, onCancel) => {
    state.cancelled = true;
    if (playbackState.get(sampler) === state) cancelPlayback(sampler);
    onCancel();
  };
};

export const stop = (sampler) => () => cancelPlayback(sampler);
