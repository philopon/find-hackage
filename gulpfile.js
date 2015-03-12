var gulp = require('gulp');
var ngmin = require('gulp-ngmin');
var uglifyjs = require('gulp-uglifyjs');
var uglifycss = require('gulp-uglifycss');
var rename = require('gulp-rename');

gulp.task('js', function(){
  return gulp
    .src('static/js/main.js')
    .pipe(ngmin())
    .pipe(uglifyjs())
    .pipe(rename({extname: '.min.js'}))
    .pipe(gulp.dest('static/js'));
});

gulp.task('css', function(){
  return gulp
    .src('static/css/main.css')
    .pipe(uglifycss())
    .pipe(rename({extname: '.min.css'}))
    .pipe(gulp.dest('static/css'));
});

gulp.task('default', ['js', 'css']);
