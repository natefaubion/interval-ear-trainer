import sharp from "sharp";

const source = "public/icons/app-icon.svg";
const outputs = [
  ["public/icons/app-icon-master.png", 1254],
  ["public/icons/icon-512.png", 512],
  ["public/icons/icon-maskable-512.png", 512],
  ["public/icons/icon-192.png", 192],
  ["public/icons/apple-touch-icon.png", 180],
  ["public/icons/favicon-64.png", 64],
];

await Promise.all(
  outputs.map(([path, size]) =>
    sharp(source, { density: 300 })
      .resize(size, size)
      .png()
      .toFile(path)
  )
);

console.log("Generated application icons.");
