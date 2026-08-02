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

## Installation and offline use

The production site is an installable Progressive Web App. After the first complete online load, the application and piano samples are cached for offline practice.

- On iPhone or iPad, open the deployed site, use the Share menu, and choose **Add to Home Screen**.
- On Android or desktop Chromium browsers, use the browser's **Install** action when available.

Microphone access must still be granted on each browser or installed-app environment. Application updates are downloaded in the background and are used the next time the application is opened or refreshed.

## Commands

- `npm run format` formats PureScript sources.
- `npm run test` runs focused domain and state-machine tests.
- `npm run licenses` regenerates the deployed third-party notices page.
- `npm run licenses:check` verifies that the committed notices match the locked runtime dependencies.
- `npm run build` generates the PureScript browser module with purs-backend-es, then creates the production site in `dist/` with Vite.
- `npm run validate` checks formatting, tests, and the production build.

## Workspace

The Spago monorepo contains the Halogen application and separate packages for music theory, quiz state, notation, audio playback, and pitch detection.

## Audio samples

Piano playback uses the locally hosted Salamander Grand Piano V3 samples by Alexander Holm. The vendored velocity-8 OGG set and its CC BY 3.0 attribution are in `public/audio/salamander/`.

## License

Copyright © 2026 Nathan Faubion.

The original source code in this repository is licensed under the GNU General Public License, version 3 or any later version. See [LICENSE](LICENSE).

Third-party software and vendored assets retain their respective licenses. In particular, the Salamander Grand Piano samples are licensed under CC BY 3.0. See the generated third-party notices and the attribution files alongside the samples for details.
