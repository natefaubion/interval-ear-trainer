import {
  Accidental,
  Formatter,
  Renderer,
  Stave,
  StaveNote,
  Voice,
} from "vexflow";

export const renderNotesImpl = (element) => (notes) => () => {
  element.replaceChildren();

  const width = 720;
  const height = 190;
  const renderer = new Renderer(element, Renderer.Backends.SVG);
  renderer.resize(width, height);

  const context = renderer.getContext();
  const stave = new Stave(12, 28, width - 24);
  stave.addClef("treble").setContext(context).draw();

  const staveNotes = notes.map(({ key, accidental }) => {
    const note = new StaveNote({ clef: "treble", keys: [key], duration: "q" });
    if (accidental !== "") note.addModifier(new Accidental(accidental), 0);
    return note;
  });

  const voice = new Voice({ numBeats: staveNotes.length, beatValue: 4 }).addTickables(staveNotes);
  new Formatter().joinVoices([voice]).format([voice], width - 180);
  voice.draw(context, stave);

  const svg = element.querySelector("svg");
  if (svg) {
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.removeAttribute("width");
    svg.removeAttribute("height");
  }
};
