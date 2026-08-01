import {
  Accidental,
  Formatter,
  Renderer,
  Stave,
  StaveNote,
  Voice,
} from "vexflow";

export const renderScoreImpl = (element) => (clef) => (width) => (events) => () => {
  element.replaceChildren();

  const height = 190;
  const renderer = new Renderer(element, Renderer.Backends.SVG);
  renderer.resize(width, height);

  const context = renderer.getContext();
  const stave = new Stave(8, 28, width - 16);
  stave.addClef(clef).setContext(context).draw();

  const staveNotes = events.map((event) => {
    const note = new StaveNote({
      clef,
      keys: event.notes.map(({ key }) => key),
      duration: "q",
    });
    event.notes.forEach(({ accidental }, index) => {
      if (event.appearance !== "hidden" && accidental !== "") {
        note.addModifier(new Accidental(accidental), index);
      }
    });
    if (event.appearance === "dim") {
      note.setStyle({ fillStyle: "#aeb4b0", strokeStyle: "#aeb4b0" });
    } else if (event.appearance === "hidden") {
      note.setStyle({ fillStyle: "transparent", strokeStyle: "transparent" });
    }
    return note;
  });

  const voice = new Voice({ numBeats: staveNotes.length, beatValue: 4 }).addTickables(staveNotes);
  new Formatter().joinVoices([voice]).format([voice], width - 105);
  voice.draw(context, stave);

  const svg = element.querySelector("svg");
  if (svg) {
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.removeAttribute("width");
    svg.removeAttribute("height");
  }
};
