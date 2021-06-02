const gulp = require("gulp");
const sass = require("gulp-sass");
const cleanCSS = require("gulp-clean-css");
const concatCss = require("gulp-concat-css");

sass.compiler = require("sass");

const sassConf = {
  source: "./src/**/*.scss",
  destination: "./public/dist",
};

function sassBuild() {
  return gulp
    .src(sassConf.source)
    .pipe(sass().on("error", sass.logError))
    .pipe(concatCss("styles.css"))
    .pipe(cleanCSS({ level: 2 }))
    .pipe(gulp.dest(sassConf.destination));
}

exports.sassBuild = sassBuild;