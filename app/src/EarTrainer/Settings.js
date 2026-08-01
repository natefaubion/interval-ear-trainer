const storageKey = "ear-trainer.settings.v1";

const isStoredSettings = (value) =>
  value !== null &&
  typeof value === "object" &&
  Array.isArray(value.intervals) &&
  value.intervals.every((item) => typeof item === "string") &&
  typeof value.octavePolicy === "string" &&
  Array.isArray(value.playbackModes) &&
  value.playbackModes.every((item) => typeof item === "string") &&
  Array.isArray(value.rootPitchClasses) &&
  value.rootPitchClasses.every(
    (item) =>
      item !== null &&
      typeof item === "object" &&
      typeof item.accidental === "number" &&
      typeof item.letter === "string",
  ) &&
  typeof value.vocalRange === "string";

export const loadImpl = (just) => (nothing) => () => {
  try {
    const value = JSON.parse(localStorage.getItem(storageKey));
    return isStoredSettings(value)
      ? just({
          ...value,
          answerDisplay: typeof value.answerDisplay === "string" ? value.answerDisplay : "notation",
          ghostMode: typeof value.ghostMode === "string" ? value.ghostMode : "on",
        })
      : nothing;
  } catch {
    return nothing;
  }
};

export const saveImpl = (settings) => () => {
  try {
    localStorage.setItem(storageKey, JSON.stringify(settings));
  } catch {
    // Storage may be unavailable in private or restricted browsing contexts.
  }
};
