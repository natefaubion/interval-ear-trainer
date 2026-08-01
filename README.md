# Ear Trainer

A browser-based interval ear-training application built with PureScript and Halogen. The learner hears an interval, sings both component notes in sequence, and identifies the interval from musical notation.

## Requirements

- Node.js 22 or newer
- npm 10 or newer

All other tools, including PureScript, Spago, purs-tidy, and purs-backend-es, are pinned project-local npm dependencies.

The project uses native ECMAScript modules throughout via `"type": "module"` in `package.json`.

## Development

```sh
npm install
npm run dev
```

Vite prints the development URL when it starts (normally <http://localhost:5173>).

## Commands

- `npm run format` formats PureScript sources.
- `npm run test` runs focused domain and state-machine tests.
- `npm run build` generates the PureScript browser module with purs-backend-es, then creates the production site in `dist/` with Vite.
- `npm run validate` checks formatting, tests, and the production build.

## Workspace

The Spago monorepo contains the Halogen application and separate packages for music theory, quiz state, notation, audio playback, and pitch detection.

## Audio samples

Piano playback uses the locally hosted Salamander Grand Piano V3 samples by Alexander Holm. The vendored velocity-8 OGG set and its CC BY 3.0 attribution are in `public/audio/salamander/`.
