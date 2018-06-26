const gulp = require('gulp');
const purescript = require('gulp-purescript');
const del = require('del');
const run = require('gulp-run');

const sources = [
  'src/**/*.purs',
  'test/**/*.purs',
  'bower_components/purescript-*/src/**/*.purs',
];

var distFiles = [
  'index.html',
  '*.css'
];

var staticDir = '../../dist/static/';


function clean () {
    return del(['output', '.pulp-cache']);
}
clean.description = 'Clean the output and .pulp-cache directories';
exports.clean = clean;


exports.fullClean = gulp.series(clean, function () {
});
exports.fullClean.description = 'Clean also npm and bower artifacts';



function compile () {
    return purescript.compile({ src: sources });
}

function bundle () {
  return purescript.bundle(
    { src: 'output/**/*.js'
    , module: 'Main'
    , main: 'Main'
    , output: staticDir + 'app.js' });
}

function test () {
    return purescript.bundle({ src: 'output/**/*.js', main: 'Test.Main' })
        .pipe(run('node'));
}

exports.test = gulp.series(compile, test);
exports.test.description = 'Run the purescript tests.';

function dist () {
    return gulp.src(distFiles)
        .pipe(gulp.dest(staticDir));
}

exports.default = gulp.parallel(gulp.series(compile, bundle), dist);
exports.default.description = 'Compile and copy to server directory.';

function watchPurescript () {
    return gulp.watch(sources, gulp.series(compile, bundle));
}

function watchDist () {
    return gulp.watch(distFiles, dist);
}

exports.watch = gulp.series(exports.default, gulp.parallel(watchPurescript, watchDist));
exports.watch.description = 'Watch input files and recompile.';
