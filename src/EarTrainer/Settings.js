const databaseName = "interval-ear-trainer";
const databaseVersion = 1;
const storeName = "application";
const stateKey = "state";

const canceler = (_error, _onError, onSuccess) => onSuccess();

const openDatabase = () =>
  new Promise((resolve, reject) => {
    const request = indexedDB.open(databaseName, databaseVersion);
    request.onerror = () => reject(request.error ?? new Error("Could not open application storage."));
    request.onupgradeneeded = () => {
      const database = request.result;
      if (!database.objectStoreNames.contains(storeName)) database.createObjectStore(storeName);
    };
    request.onsuccess = () => resolve(request.result);
  });

const readState = async () => {
  const database = await openDatabase();
  try {
    return await new Promise((resolve, reject) => {
      const transaction = database.transaction(storeName, "readonly");
      const request = transaction.objectStore(storeName).get(stateKey);
      request.onerror = () => reject(request.error ?? new Error("Could not read application storage."));
      request.onsuccess = () => resolve(request.result);
    });
  } finally {
    database.close();
  }
};

const writeState = async (state) => {
  const database = await openDatabase();
  try {
    await new Promise((resolve, reject) => {
      const transaction = database.transaction(storeName, "readwrite");
      transaction.objectStore(storeName).put(state, stateKey);
      transaction.onabort = () => reject(transaction.error ?? new Error("Could not save application storage."));
      transaction.onerror = () => reject(transaction.error ?? new Error("Could not save application storage."));
      transaction.oncomplete = () => resolve();
    });
  } finally {
    database.close();
  }
};

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

const withSettingsDefaults = (value) => ({
  ...value,
  answerCount: typeof value.answerCount === "string" ? value.answerCount : "few",
  answerDisplay: typeof value.answerDisplay === "string" ? value.answerDisplay : "notation",
  availableIntervals: Array.isArray(value.availableIntervals)
    ? value.availableIntervals
    : ["third", "fourth", "fifth", "octave"],
  customLowMidi: Number.isInteger(value.customLowMidi) ? value.customLowMidi : 48,
  customHighMidi: Number.isInteger(value.customHighMidi) ? value.customHighMidi : 79,
  ghostMode: typeof value.ghostMode === "string" ? value.ghostMode : "on",
  intervalSystem: typeof value.intervalSystem === "string" ? value.intervalSystem : "from-selected-notes",
  showPitchTuner: typeof value.showPitchTuner === "boolean" ? value.showPitchTuner : true,
  quizMode: typeof value.quizMode === "string" ? value.quizMode : "singing-and-recognition",
  quizProgression: typeof value.quizProgression === "string" ? value.quizProgression : "automatic",
});

const validateState = (value) => {
  if (
    value === null ||
    typeof value !== "object" ||
    !isStoredSettings(value.settings) ||
    !Array.isArray(value.presets)
  ) return null;

  const presets = value.presets
    .filter(
      (preset) =>
        preset !== null &&
        typeof preset === "object" &&
        typeof preset.id === "string" &&
        typeof preset.name === "string" &&
        isStoredSettings(preset.settings),
    )
    .map((preset) => ({ ...preset, settings: withSettingsDefaults(preset.settings) }));

  const activePresetId =
    typeof value.activePresetId === "string" && presets.some((preset) => preset.id === value.activePresetId)
      ? value.activePresetId
      : "";

  return { activePresetId, presets, settings: withSettingsDefaults(value.settings) };
};

export const loadImpl = (just) => (nothing) => (onError, onSuccess) => {
  readState()
    .then((value) => {
      const state = validateState(value);
      onSuccess(state === null ? nothing : just(state));
    })
    .catch(onError);
  return canceler;
};

export const saveImpl = (state) => (onError, onSuccess) => {
  writeState(state).then(() => onSuccess()).catch(onError);
  return canceler;
};

export const requestPersistenceImpl = (onError, onSuccess) => {
  Promise.resolve()
    .then(async () => {
      if (!navigator.storage?.persist) return false;
      if (await navigator.storage.persisted?.()) return true;
      return navigator.storage.persist();
    })
    .then(onSuccess)
    .catch(onError);
  return canceler;
};

export const newPresetId = () =>
  globalThis.crypto?.randomUUID?.() ?? `${Date.now()}-${Math.random().toString(36).slice(2)}`;
