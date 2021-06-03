const fs = require("fs/promises");
const path = require("path");
const chokidar = require("chokidar");
const sass = require("sass");
const util = require("util");
const globAsync = util.promisify(require("glob"));
const concat = require("concat");

const TEMPDIR = ".tempcss";
const OUTFILE = "public/dist/styles.css";

async function createTempDir() {
  await fs.rm(TEMPDIR, { recursive: true, force: true });
  await fs.mkdir(TEMPDIR, { recursive: true });
}

async function startSassWatch() {
  const sassFiles = chokidar.watch("src/**/*.sass", { persistent: true });
  sassFiles.on("all", (event, filePath) => {
    switch (event) {
      case "add":
      case "change":
        return create(filePath);
      case "unlink":
      case "unlinkDir":
        return destroy(filePath);
    }
  });
}

async function create(filePath) {
  const { dir, name } = path.parse(filePath);
  const outDir = dir.replace(/^src/, TEMPDIR);
  const outputPath = path.format({ dir: outDir, name, ext: ".css" });
  const res = sass.renderSync({ file: filePath, outFile: outputPath });
  await fs.mkdir(outDir, { recursive: true });
  await fs.writeFile(outputPath, res.css);
}

async function destroy(filePath) {
  await fs.rm(filePath, { recursive: true, force: true });
}

async function startCssConcatWatch() {
  const cssFiles = chokidar.watch(`${TEMPDIR}/**/*.css`, { persistent: true });
  cssFiles.on("all", (event) => {
    switch (event) {
      case "add":
      case "change":
      case "unlink":
      case "unlinkDir":
        concatAllCss();
    }
  });
}

async function concatAllCss() {
  const matches = await globAsync(`${TEMPDIR}/**/*.css`);
  await concat(matches, OUTFILE);
  console.log(`${new Date().toISOString()} run`)
}

async function main() {
  await createTempDir();
  await startSassWatch();
  await startCssConcatWatch();
}

main();
