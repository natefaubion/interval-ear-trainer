import { existsSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const projectRoot = new URL("../", import.meta.url).pathname;
const outputPath = join(projectRoot, "public", "third-party-notices.html");
const deployedLicensePath = join(projectRoot, "public", "LICENSE.txt");
const checkOnly = process.argv.includes("--check");

const escapeHtml = (value) =>
  value
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");

const licenseFile = (directory, declaredLicense) => {
  const filename = readdirSync(directory).find((entry) => /^(licen[cs]e|copying)(\..*)?$/i.test(entry));
  if (filename) return readFileSync(join(directory, filename), "utf8").trim().replaceAll("\r\n", "\n");

  const readmeFilename = readdirSync(directory).find((entry) => /^readme(\..*)?$/i.test(entry));
  if (readmeFilename) {
    const readme = readFileSync(join(directory, readmeFilename), "utf8").replaceAll("\r\n", "\n");
    const licenseHeading = readme.search(/^#{1,6}\s+licen[cs]e\s*$/im);
    if (licenseHeading >= 0) {
      const section = readme.slice(licenseHeading).replace(/^#{1,6}\s+licen[cs]e\s*\n+/i, "").trim();
      if (section) return section;
    }
  }

  if (declaredLicense) {
    return `License declared by the package: ${declaredLicense}\n\nThe published package archive does not include separate license text.`;
  }
  throw new Error(`No license text or declared license found in ${directory}`);
};

const npmPackages = () => {
  const lock = JSON.parse(readFileSync(join(projectRoot, "package-lock.json"), "utf8"));
  const packages = new Map();

  const visit = (dependencies = {}) => {
    for (const name of Object.keys(dependencies)) {
      const lockPath = `node_modules/${name}`;
      const dependency = lock.packages[lockPath];
      if (!dependency) throw new Error(`npm dependency is missing from package-lock.json: ${name}`);
      const key = `${name}@${dependency.version}`;
      if (!packages.has(key)) {
        const directory = join(projectRoot, lockPath);
        const manifest = JSON.parse(readFileSync(join(directory, "package.json"), "utf8"));
        packages.set(key, {
          name,
          version: dependency.version,
          license: manifest.license ?? "See license text",
          text: licenseFile(directory, manifest.license),
        });
      }
      visit(dependency.dependencies);
    }
  };

  visit(lock.packages[""].dependencies);
  return [...packages.values()].sort((left, right) => left.name.localeCompare(right.name));
};

const spagoPackages = () => {
  const lock = JSON.parse(readFileSync(join(projectRoot, "spago.lock"), "utf8"));
  const selected = new Set();
  const visit = (name) => {
    if (selected.has(name)) return;
    selected.add(name);
    const workspacePackage = lock.workspace.packages[name];
    const dependencies = workspacePackage?.core.dependencies ?? lock.packages[name]?.dependencies ?? [];
    dependencies.forEach(visit);
  };
  visit("ear-trainer");

  return [...selected]
    .filter((name) => lock.packages[name]?.type === "registry")
    .map((name) => {
      const version = lock.packages[name].version;
      const directory = join(projectRoot, ".spago", "p", `${name}-${version}`);
      if (!existsSync(directory)) throw new Error(`Spago package is not downloaded: ${name}@${version}`);
      const manifestPath = join(directory, "purs.json");
      const manifest = existsSync(manifestPath) ? JSON.parse(readFileSync(manifestPath, "utf8")) : {};
      return {
        name,
        version,
        license: manifest.license ?? "See license text",
        text: licenseFile(directory, manifest.license),
      };
    })
    .sort((left, right) => left.name.localeCompare(right.name));
};

const groupedLicenses = (packages) => {
  const groups = new Map();
  for (const dependency of packages) {
    const current = groups.get(dependency.text) ?? [];
    current.push(`${dependency.name}@${dependency.version}`);
    groups.set(dependency.text, current);
  }
  return [...groups.entries()].sort(([, left], [, right]) => left[0].localeCompare(right[0]));
};

const renderGroups = (title, packages) => `
  <section>
    <h2>${escapeHtml(title)}</h2>
    ${groupedLicenses(packages)
      .map(
        ([text, names]) => `<article>
      <h3>${escapeHtml(names.join(", "))}</h3>
      <pre>${escapeHtml(text)}</pre>
    </article>`,
      )
      .join("\n")}
  </section>`;

const html = `<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="theme-color" content="#f4efe5" />
    <title>Third-party notices · Interval Ear Trainer</title>
    <style>
      :root { color: #27312d; background: #f4efe5; font-family: system-ui, sans-serif; }
      body { max-width: 52rem; margin: 0 auto; padding: clamp(1.25rem, 5vw, 3rem); line-height: 1.55; }
      h1, h2 { font-family: Georgia, serif; font-weight: 500; }
      h1 { margin-top: 0; }
      h2 { margin-top: 2.5rem; padding-top: 1rem; border-top: 1px solid #d7d0c5; }
      h3 { font-size: 0.95rem; overflow-wrap: anywhere; }
      a { color: #536f67; text-underline-offset: 0.14em; }
      article { margin-top: 1.5rem; }
      pre { overflow-x: auto; padding: 1rem; border: 1px solid #ded9d0; border-radius: 0.6rem; background: #faf8f3; white-space: pre-wrap; font: 0.78rem/1.5 ui-monospace, monospace; }
    </style>
  </head>
  <body>
    <p><a href="./">← Back to Interval Ear Trainer</a></p>
    <h1>Third-party notices</h1>
    <p>This application includes open-source software and audio made available under the licenses below.</p>
    <section>
      <h2>Audio samples</h2>
      <p><a href="https://archive.org/details/SalamanderGrandPianoV3">Salamander Grand Piano V3</a> by Alexander Holm is licensed under <a href="https://creativecommons.org/licenses/by/3.0/">Creative Commons Attribution 3.0</a>. The application includes the velocity-8 MP3 sample set.</p>
    </section>
    ${renderGroups("JavaScript packages", npmPackages())}
    ${renderGroups("PureScript packages", spagoPackages())}
  </body>
</html>
`;
const projectLicense = readFileSync(join(projectRoot, "LICENSE"), "utf8");

if (checkOnly) {
  if (
    !existsSync(outputPath)
    || readFileSync(outputPath, "utf8") !== html
    || !existsSync(deployedLicensePath)
    || readFileSync(deployedLicensePath, "utf8") !== projectLicense
  ) {
    console.error("Deployed license notices are out of date. Run `npm run licenses`.");
    process.exitCode = 1;
  } else {
    console.log("Deployed license notices are current.");
  }
} else {
  writeFileSync(outputPath, html);
  writeFileSync(deployedLicensePath, projectLicense);
  console.log(`Wrote ${outputPath} and ${deployedLicensePath}`);
}
