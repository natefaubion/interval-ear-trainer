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

export const loadImpl = (just) => (nothing) => (onError, onSuccess) => {
  readState()
    .then((value) => onSuccess(value == null ? nothing : just(value)))
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
