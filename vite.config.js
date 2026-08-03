import { defineConfig } from "vite";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig({
  base: "./",
  plugins: [
    VitePWA({
      injectRegister: "auto",
      registerType: "autoUpdate",
      includeAssets: [
        "icons/apple-touch-icon.png",
        "icons/favicon-64.png",
        "LICENSE.txt",
        "third-party-notices.html",
      ],
      manifest: {
        id: "./",
        name: "Interval Ear Trainer",
        short_name: "Ear Trainer",
        description: "Practice singing and identifying musical intervals from notation.",
        start_url: "./",
        scope: "./",
        display: "standalone",
        orientation: "portrait-primary",
        background_color: "#f4efe5",
        theme_color: "#f4efe5",
        categories: [ "education", "music" ],
        icons: [
          {
            src: "icons/icon-192.png",
            sizes: "192x192",
            type: "image/png",
            purpose: "any",
          },
          {
            src: "icons/icon-512.png",
            sizes: "512x512",
            type: "image/png",
            purpose: "any",
          },
          {
            src: "icons/icon-maskable-512.png",
            sizes: "512x512",
            type: "image/png",
            purpose: "maskable",
          },
        ],
      },
      workbox: {
        cleanupOutdatedCaches: true,
        globPatterns: [ "**/*.{css,html,js,ogg,png,svg,txt}" ],
        navigateFallback: "index.html",
      },
    }),
  ],
});
