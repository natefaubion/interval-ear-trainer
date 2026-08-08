const databaseName = "interval-ear-trainer";
const databaseVersion = 1;
const storeName = "application";
const stateKey = "state";

const fromPromise = (promise, onError, onSuccess) => {
  let cancelled = false;
  promise.then(
    (value) => {
      if (!cancelled) onSuccess(value);
    },
    (error) => {
      if (!cancelled) onError(error);
    },
  );
  return (_error, _onError, onCancel) => {
    cancelled = true;
    onCancel();
  };
};

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
  return fromPromise(
    readState().then((value) => (value == null ? nothing : just(value))),
    onError,
    onSuccess,
  );
};

export const saveImpl = (state) => (onError, onSuccess) =>
  fromPromise(writeState(state), onError, onSuccess);

export const requestPersistenceImpl = (onError, onSuccess) => {
  return fromPromise(
    Promise.resolve().then(async () => {
      if (!navigator.storage?.persist) return false;
      if (await navigator.storage.persisted?.()) return true;
      return navigator.storage.persist();
    }),
    onError,
    onSuccess,
  );
};
