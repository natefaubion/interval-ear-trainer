export const setTypeImpl = (type) => () => {
  if ("audioSession" in navigator) navigator.audioSession.type = type;
};
