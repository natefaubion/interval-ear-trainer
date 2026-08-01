import * as Tone from "tone";

const sampleUrls = {
  A0: "A0v8.ogg",
  A1: "A1v8.ogg",
  A2: "A2v8.ogg",
  A3: "A3v8.ogg",
  A4: "A4v8.ogg",
  A5: "A5v8.ogg",
  A6: "A6v8.ogg",
  A7: "A7v8.ogg",
  C1: "C1v8.ogg",
  C2: "C2v8.ogg",
  C3: "C3v8.ogg",
  C4: "C4v8.ogg",
  C5: "C5v8.ogg",
  C6: "C6v8.ogg",
  C7: "C7v8.ogg",
  C8: "C8v8.ogg",
  "D#1": "D%231v8.ogg",
  "D#2": "D%232v8.ogg",
  "D#3": "D%233v8.ogg",
  "D#4": "D%234v8.ogg",
  "D#5": "D%235v8.ogg",
  "D#6": "D%236v8.ogg",
  "D#7": "D%237v8.ogg",
  "F#1": "F#1v8.ogg",
  "F#2": "F#2v8.ogg",
  "F#3": "F#3v8.ogg",
  "F#4": "F#4v8.ogg",
  "F#5": "F#5v8.ogg",
  "F#6": "F#6v8.ogg",
  "F#7": "F#7v8.ogg",
};

const sampleBaseUrl = new URL("./audio/salamander/", document.baseURI).href;
const midiNote = (midi) => Tone.Frequency(midi, "midi").toNote();
const samplerReady = new WeakMap();

export const createSampler = () => {
  let markReady;
  const ready = new Promise((resolve) => {
    markReady = resolve;
  });
  const sampler = new Tone.Sampler({
    urls: sampleUrls,
    baseUrl: sampleBaseUrl,
    release: 0.7,
    onload: markReady,
  }).toDestination();
  samplerReady.set(sampler, ready);
  return sampler;
};

export const playIntervalImpl = (sampler) => (rootMidi) => (targetMidi) => (mode) => () => {
  void (async () => {
    await Tone.start();
    await samplerReady.get(sampler);

    sampler.releaseAll();
    const root = midiNote(rootMidi);
    const target = midiNote(targetMidi);
    const now = Tone.now() + 0.05;

    if (mode === "harmonic") {
      sampler.triggerAttackRelease([root, target], 0.9, now);
    } else {
      sampler.triggerAttackRelease(root, 0.65, now);
      sampler.triggerAttackRelease(target, 0.65, now + 0.8);
    }
  })();
};

export const stop = (sampler) => () => sampler.releaseAll();
