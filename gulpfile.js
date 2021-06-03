const { src, dest, parallel } = require("gulp");
const sass = require("gulp-sass");
const cleanCSS = require("gulp-clean-css");
const concatCss = require("gulp-concat-css");
const gulpBrotli = require("gulp-brotli");
const gulpGzip = require("gulp-gzip");

sass.compiler = require("sass");

const sassConf = {
  source: "./src/**/*.scss",
  destination: "./public/dist",
};

function sassBuild() {
  return src(sassConf.source)
    .pipe(sass().on("error", sass.logError))
    .pipe(concatCss("styles.css"))
    .pipe(cleanCSS({ level: 2 }))
    .pipe(dest(sassConf.destination));
}

const optimizationOptions = {
  source: [
    "./public/**/*.html",
    "./public/**/*.js",
    "./public/**/*.css",
    "./public/**/*.svg",
  ],
  destination: "./public",
};

function brotli() {
  return src(optimizationOptions.source)
    .pipe(gulpBrotli({ skipLarger: true }))
    .pipe(dest(optimizationOptions.destination));
}

function gzip() {
  return src(optimizationOptions.source)
    .pipe(gulpGzip({ skipGrowingFiles: true, gzipOptions: { level: 9 } }))
    .pipe(dest(optimizationOptions.destination));
}

exports.sassBuild = sassBuild;
exports.optimize = parallel(brotli, gzip);
