var gulp = require('gulp'),
  elm = require('gulp-elm'),
  rename = require('gulp-rename');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function () {
  return gulp.src('./Bingo.elm')
    .pipe(elm())
    .on('error', swallowError)
    .pipe(rename('bingo.js'))
    .pipe(gulp.dest('./'))
});

function swallowError (error) {
  console.log(error.toString());
  this.emit('end');
}

gulp.task('default', ['elm'], function () {
  gulp.watch('./*.elm', ['elm']);
});