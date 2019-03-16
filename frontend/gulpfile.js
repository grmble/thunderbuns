const gulp = require('gulp');
const purescript = require('gulp-purescript');
const del = require('del');
const run = require('gulp-run');
const browserSync = require('browser-sync').create();
const gzip = require('gulp-gzip');

const staticDir = 'dist/';

// note - no app.js
// javascript is in a different pipeline
const staticDirCompressAssets = [
  'dist/*.css',
  'dist/*.html',
  'dist/*.min.js',
];

const sources = [
  'src/**/*.purs',
  'test/**/*.purs',
  'bower_components/purescript-*/src/**/*.purs',
];

const distFiles = [
  'index.html',
  '*.css',
  'bower_components/toastr/toastr.min.css',
  'bower_components/toastr/toastr.min.js',
  'bower_components/jquery/dist/jquery.min.js',
];


function clean () {
  return del(['output', '.pulp-cache', staticDir]);
}
clean.description = 'Clean the output, dist and .pulp-cache directories';
exports.clean = clean;


exports.fullClean = gulp.series(clean, function () {
  return del(['node_modules', 'bower_components']);
});
exports.fullClean.description = 'Clean also npm and bower artifacts';



function compile () {
  return purescript.compile({ src: sources });
}

// bundles & zips app.js
function bundleOnly() {
  return purescript.bundle(
    { src: 'output/**/*.js'
      , module: 'Main'
      , main: 'Main'
      , output: staticDir + 'app.js' });
}

function compressBundle() {
  return gulp.src(staticDir + 'app.js')
    .pipe(gzip())
    .pipe(gulp.dest(staticDir));
}

const bundle = gulp.series(bundleOnly, compressBundle);

function test () {
  return purescript.bundle({ src: 'output/**/*.js', main: 'Test.Main' })
    .pipe(run('node'));
}

exports.test = gulp.series(compile, test);
exports.test.description = 'Run the purescript tests.';

function distOnly () {
  return gulp.src(distFiles)
    .pipe(gulp.dest(staticDir));
}

function compressDist () {
  return gulp.src(staticDirCompressAssets)
    .pipe(gzip())
    .pipe(gulp.dest(staticDir));
}

const dist = gulp.series(distOnly, compressDist);

exports.default = gulp.parallel(gulp.series(compile, bundle),
                                gulp.series(dist));
exports.default.description = 'Compile and copy to dist directory.';

function watchPurescript () {
  return gulp.watch(sources, gulp.series(compile, bundle));
}

function watchPurescriptReload () {
  return gulp.watch(sources, gulp.series(compile, bundle, reload));
}

function watchDist () {
  return gulp.watch(distFiles, dist);
}

function watchDistReload() {
  return gulp.watch(distFiles, gulp.series(dist, reload));
}

exports.watch = gulp.series(exports.default, gulp.parallel(watchPurescript, watchDist));
exports.watch.description = 'Watch input files and recompile.';

function reload(done) {
  browserSync.reload();
  done();
}

function runProxy(done) {
  browserSync.init({proxy: 'http://localhost:1337/',
                    startPath: 'index.html',
                    reloadOnRestart: true
                   });
  done();
}

exports.serve =
  gulp.series(
    exports.default,
    runProxy,
    gulp.parallel(watchPurescriptReload, watchDistReload));
exports.serve.description = "Start BrowserSync proxy and watch for file changes.";
