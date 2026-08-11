import {
  Accidental,
  Annotation,
  AnnotationVerticalJustify,
  Formatter,
  Renderer,
  Stave,
  StaveNote,
  Voice,
} from "vexflow";

// VexFlow owns its SVG nodes; PureScript supplies a complete score value.

const svgNamespace = "http://www.w3.org/2000/svg";
let renderSequence = 0;

const scoreVerticalBounds = (svg, layout) => {
  if (layout === "full") return { top: 0, height: 190 };

  let top = 20;
  let bottom = 124;

  // VexFlow's transparent hit rectangle tracks the rendered note geometry
  // without the oversized font metrics reported by its notehead text.
  svg.querySelectorAll(".vf-stavenote > rect").forEach((rect) => {
    const y = Number(rect.getAttribute("y"));
    const height = Number(rect.getAttribute("height"));
    top = Math.min(top, y - 14);
    bottom = Math.max(bottom, y + height + 14);
  });

  return { top, height: bottom - top };
};

const addRecognizedFilter = (svg) => {
  const filterId = `recognized-note-halo-${renderSequence++}`;
  const defs = document.createElementNS(svgNamespace, "defs");
  const filter = document.createElementNS(svgNamespace, "filter");
  const blur = document.createElementNS(svgNamespace, "feGaussianBlur");

  filter.setAttribute("id", filterId);
  filter.setAttribute("x", "-100%");
  filter.setAttribute("y", "-100%");
  filter.setAttribute("width", "300%");
  filter.setAttribute("height", "300%");
  blur.setAttribute("stdDeviation", "4");
  filter.appendChild(blur);
  defs.appendChild(filter);
  svg.insertBefore(defs, svg.firstChild);

  return filterId;
};

const addRecognizedHighlight = (group, filterId) => {
  const highlightColor = "#f4bd00";
  const shadow = group.cloneNode(true);
  shadow.removeAttribute("id");
  shadow.setAttribute("aria-hidden", "true");
  shadow.setAttribute("fill", highlightColor);
  shadow.setAttribute("stroke", highlightColor);
  shadow.setAttribute("filter", `url(#${filterId})`);
  shadow.setAttribute("opacity", "0.95");
  shadow.style.pointerEvents = "none";

  shadow.querySelectorAll("*").forEach((element) => {
    const fill = element.getAttribute("fill");
    const stroke = element.getAttribute("stroke");
    if (fill && fill !== "none" && fill !== "transparent") {
      element.setAttribute("fill", highlightColor);
    }
    if (stroke && stroke !== "none" && stroke !== "transparent") {
      element.setAttribute("stroke", highlightColor);
    }
    element.removeAttribute("id");
  });

  group.parentNode.insertBefore(shadow, group);
};

export const renderScoreImpl = (element) => (layout) => (clef) => (width) => (events) => () => {
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
    event.notes.forEach(({ accidental, octaveMark, octavePosition }, index) => {
      if (!event.hidden && accidental !== "") {
        note.addModifier(new Accidental(accidental), index);
      }
      if (octaveMark !== "") {
        const annotation = new Annotation(octaveMark).setVerticalJustification(
          octavePosition === "top"
            ? AnnotationVerticalJustify.TOP
            : AnnotationVerticalJustify.BOTTOM,
        );
        if (event.color !== "") annotation.setStyle({ fillStyle: event.color });
        note.addModifier(annotation, index);
      }
    });
    if (event.color !== "") {
      note.setStyle({ fillStyle: event.color, strokeStyle: event.color });
    }
    if (event.hidden) {
      note.setLedgerLineStyle({ fillStyle: "transparent", strokeStyle: "transparent" });
    }
    return note;
  });

  const voice = new Voice({ numBeats: staveNotes.length, beatValue: 4 }).addTickables(staveNotes);
  const formatWidth = events.length >= 3 ? width - 110 : width - 140;
  new Formatter().joinVoices([voice]).format([voice], formatWidth);
  voice.draw(context, stave);

  const svg = element.querySelector("svg");
  if (svg) {
    const noteGroups = svg.querySelectorAll(".vf-stavenote");
    const verticalBounds = scoreVerticalBounds(svg, layout);
    const recognizedFilterId = addRecognizedFilter(svg);
    events.forEach((event, index) => {
      if (event.highlighted) {
        const group = noteGroups[index];
        if (group) {
          addRecognizedHighlight(group, recognizedFilterId);
        }
      }
    });

    svg.setAttribute("viewBox", `0 ${verticalBounds.top} ${width} ${verticalBounds.height}`);
    svg.removeAttribute("width");
    svg.removeAttribute("height");
    svg.style.removeProperty("width");
    svg.style.removeProperty("height");
  }
  element.style.removeProperty("width");
  element.style.removeProperty("height");
};
