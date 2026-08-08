# Ear Trainer Contributor Guide

This is a browser-based interval ear trainer written in PureScript with Halogen. Prefer precise domain models, small modules, and pure logic. JavaScript exists to bind JavaScript libraries and browser APIs; it is not the application's implementation language.

## Project structure

The repository is one Spago application package. PureScript sources live under `src/` and tests live under `test/`. Do not split application modules into separate packages merely to express module boundaries.

Keep this file current with the repository. Changes to paths, commands, architecture, or contributor expectations must update these instructions in the same change.

The intended module boundaries are:

- `EarTrainer.Music`: pitches, intervals, transposition, and music-theory operations.
- `EarTrainer.Config`: exercise configuration and validation.
- `EarTrainer.Quiz`: prompt and answer generation.
- `EarTrainer.Recognition`: pure pitch-observation and recognition state machines.
- `EarTrainer.Component.App`: application routing and shared resource ownership.
- `EarTrainer.Component.Setup`: configuration and preset editing.
- `EarTrainer.Component.Practice`: one practice session and its transitions.
- `EarTrainer.Component.Notation`: the Halogen boundary for rendered notation.
- `EarTrainer.UI.*`: reusable, mostly stateless HTML constructors such as buttons, panels, dialogs, and setting groups.
- `EarTrainer.Capability.*`: thin browser and JavaScript-library bindings.
- `EarTrainer.Settings.*`: settings model, codecs, migrations, and persistence coordination.

`Main` should remain a minimal program entry point.

## PureScript first

- Put domain rules, validation, state transitions, scheduling decisions, data conversion, filtering, and error handling in PureScript.
- Keep FFI modules small and capability-oriented. FFI may wrap Tone, Pitchy, VexFlow, IndexedDB, Web Audio, media capture, and other browser primitives.
- Do not put use-case orchestration, application defaults, schema migration, or business state machines in JavaScript.
- Do not add a JavaScript helper merely because an operation is easier to express there.
- Use opaque foreign types for library-owned handles. Convert typed PureScript values to library representations at the narrowest boundary.
- Replace string protocols with ADTs in PureScript. Encoding strings for a foreign library or persisted format belongs in a boundary module.
- Model asynchronous operations as cancellable `Aff` actions. Use structured acquisition and cleanup so audio, microphone, animation frame, timer, subscription, and fiber lifetimes are finalized correctly when a component changes phase or is finalized.

When modifying existing FFI, look for policy that can be pulled back into PureScript. Avoid broad rewrites of stable library glue without tests or a concrete architectural benefit.

## Model states precisely

- Prefer a sum type over several related Boolean or `Maybe` fields.
- Make illegal combinations unrepresentable. Phase-specific data belongs in the constructor for that phase.
- Use newtypes for identifiers and numeric values that could be confused, such as MIDI notes, hertz, cents, and preset IDs.
- Functions that can fail must expose failure with `Maybe`, `Either`, or a domain-specific result. Do not silently manufacture fallback prompts for invalid configuration.
- Validation should return structured errors when callers need to explain or distinguish failures.
- Keep cancellers or fibers for in-flight work and cancel them when leaving the state that owns the work. Prefer `Aff` cancellation, `bracket`, and Halogen-managed forks to ad hoc revision or generation tokens.
- There should be one authoritative practice state machine. Do not maintain a second, nominal phase model disconnected from the Halogen state.

Keep the pure transition layer separate from effect interpretation where practical. Halogen handlers should coordinate effects around pure domain decisions rather than encode the entire state machine procedurally.

## Halogen boundaries

- Use a component when it owns state, a resource lifetime, or a meaningful parent/child protocol.
- Use typed HTML helper functions for stateless reusable presentation. Buttons and panels do not need independent components merely to share markup.
- `App` coordinates loading and navigation. `Setup` owns setup editing. `Practice` owns practice. Do not duplicate setup or practice behavior across components.
- Child inputs and outputs should form small, explicit APIs. Avoid passing a root state wholesale when a smaller input record expresses the dependency.
- A component that owns a foreign-rendered DOM node should also own the relevant ref and rendering lifecycle.
- Avoid large local component definitions and application-sized `where` blocks. Extract a module once a definition represents an independent concept.

## PureScript style

Write compact, idiomatic PureScript in the style established by Nathan Faubion's libraries:

- Prefer `case _ of` for handlers and small eliminators.
- Prefer pattern matching and guards to nested Boolean conditionals.
- Use `do` with local `let` bindings when intermediate values feed subsequent effects.
- Prefer small total functions with explicit signatures.
- Use qualified imports when they make ownership or provenance clearer.
- Give modules explicit export lists.
- Keep records narrow and named when they represent a domain concept.
- Avoid point-free code when it hides the data flow, but do use ordinary composition when it makes the code clearer.
- Do not add comments that paraphrase the code. Document invariants, surprising browser constraints, and non-obvious library behavior.
- Format with `purs-tidy`; do not hand-align code against the formatter.

Follow the existing `.tidyrc.json`. Do not introduce a new formatting or linting system without a specific need.

## Settings and persistence

- Persist an explicitly versioned representation, not the live application record by accident.
- Decode, validate, default, and migrate persisted values in PureScript.
- Treat malformed or future-version data as a typed decode failure.
- Keep IndexedDB FFI limited to opening the database and reading or writing an opaque value.
- Add migration tests before changing an existing stored representation. Existing users' saved settings and presets should survive upgrades whenever reasonably possible.

## Testing

Pure logic should be testable without a browser. Add or update tests for:

- Music and interval operations.
- Configuration validation.
- Prompt and answer generation, including invalid input.
- Every state-machine transition, cancellation path, and rejection of impossible events.
- Recognition smoothing, silence, octave policy, and thresholds.
- Settings codecs and migrations.
- Preset operations.
- Pure audio or notation plans when orchestration is separated from execution.

Keep FFI tests focused on the boundary contract. Do not duplicate library test suites.

For a normal change, run the narrowest relevant test during iteration and finish with:

```sh
npm run validate
```

If the change affects audio, microphone capture, IndexedDB, VexFlow, PWA behavior, or another browser-only facility, also report the manual browser checks performed. Do not claim those facilities were verified solely because the PureScript build passed.

## Repository hygiene

- Use the npm scripts in `package.json`; project tools are pinned locally.
- Do not edit generated artifacts in `output/`, `output-es/`, `dist/`, `app.js`, or `app.js.map`.
- Do not commit generated build artifacts unless repository policy explicitly changes.
- Preserve the vendored piano samples and their attribution.
- When dependencies change, run the license check or regeneration required by the existing npm scripts.
- Keep changes focused. Do not combine an architectural migration with unrelated UI or product changes.
- Preserve user changes in a dirty worktree and do not discard files to make a refactor easier.

## Refactoring expectations

Prefer compilable, reviewable stages over a single repository-wide rewrite. A useful order is:

1. Move modules without changing behavior.
2. Establish component and capability boundaries.
3. Add characterization tests around existing behavior.
4. Replace imprecise state with typed transitions.
5. Pull policy out of FFI.
6. Remove superseded code and package configuration.

Temporary compatibility layers are acceptable when they make a stage safe, but identify them clearly and remove them once all callers have migrated. Do not preserve dead abstractions simply because they already exist.
