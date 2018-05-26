var gulp = require("gulp");
var purescript = require("gulp-purescript");
var run = require("gulp-run");
var del = require("del");

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
];

var distFiles = [
  "index.html",
  "*.css",
  "output/app.js"
];

gulp.task("clean", function() {
  return del([ 'output', 'bower_components', 'node_modules', 'dist' ]);
});

gulp.task("clean-app.js", function() {
  return del([ 'output/app.js' ]);
});

gulp.task("compile", function () {
  return purescript.compile({ src: sources });
});

gulp.task("bundle", ["clean-app.js", "compile"], function () {
  return purescript.bundle(
    { src: "output/**/*.js"
    , module: "Main"
    , main: "Main"
    , output: "output/app.js" });
});

gulp.task("docs", function () {
  return purescript.docs({
      src: sources,
      docgen: {
        // "Name.Of.Module1": "docs/Name/Of/Module1.md",
      }
    });
});

gulp.task("test", ["compile"], function() {
  return purescript.bundle({ src: "output/**/*.js", main: "Test.Main" })
    .pipe(run("node"));
});

gulp.task("copy", ["bundle"], function() {
    return gulp.src(distFiles)
        .pipe(gulp.dest("../../dist/static"));
});

gulp.task("copy-appjs", ["bundle"], function() {
  return gulp.src("output/app.js")
    .pipe(gulp.dest("../dist/static"));
});

gulp.task("copyDemoSources", [], function() {
  return gulp.src("src/Demo/*.purs")
    .pipe(gulp.dest("dist/src/Demo/"));
});

gulp.task("dist", ["copy"
                  /* , "copyDemoSources" */
                  ]);

gulp.task("default", ["bundle", "test", "dist"]);

gulp.task("fast", ["bundle", "copy-appjs"]);

