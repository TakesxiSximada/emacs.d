// -*- coding: utf-8 -*-
var fs = require('fs');
var gulp = require('gulp');
var child_process = require('child_process');

var lib_dir = 'vendor/'
var bower_dir = 'bower_components/';

var lib = function (path){return lib_dir + path};
var bower = function (path){return bower_dir + path};
var bower_to_lib = function(options){
    for(var src in options){
        var dst = options[src];
        console.log(src + ' -> ' + dst);
        gulp.src(bower(src))
            .pipe(gulp.dest(lib(dst)));
    }
};

//--- do'nt edit me --------------
gulp.task('template', function (){
    bower_to_lib({'': ''});
});
//--------------------------------
gulp.task('font-awesome', function (){
    bower_to_lib({
        'font-awesome/css/*': 'font-awesome/css/',
        'font-awesome/fonts/*': 'font-awesome/fonts/',
    });
});

gulp.task('threejs', function (){
    bower_to_lib({'threejs/build/three.min.js': 'threejs/'});
});

gulp.task('react', function (){
    bower_to_lib({'react/*.js': 'react/'});
});


gulp.task('all', function(){
    fs.readFile('./bower.json', 'utf8', function (err, text){
        var data = JSON.parse(text);
        Object.keys(data.dependencies).forEach(function (name){
            gulp.run(name);
        });
    });
});


gulp.task('build', function (){
    var cmd = 'webpack  --progress --colors'
    var child = child_process.exec(cmd, function (err, stdout, stderr){
        if (!err){
            console.log('stdout: ' + stdout);
            console.log('stderr: ' + stderr)
        } else {
            console.log(err);
            // err.code will be the exit code of the child process
            console.log(err.code);
            // err.signal will be set to the signal that terminated the process
            console.log(err.signal);
        }
    });
});


gulp.task('test', function (){
    var status = 'stop';
    var cmd = 'nosetests';
    var targets = [
        'src/**/*.py',
        'tests/**/*.py'
    ];

    var testing = (function (event){
        console.log('run: ' +  event.path);
        if(status != 'running'){
            status = 'running';
            var child = child_process.exec(cmd, function (err, stdout, stderr){
                if (!err){
                    console.log('stdout: ' + stdout);
                    console.log('stderr: ' + stderr)
                } else {
                    console.log(err);
                    // err.code will be the exit code of the child process
                    console.log(err.code);
                    // err.signal will be set to the signal that terminated the process
                    console.log(err.signal);
                }
                status = 'stop';
            });
        };
    });

    gulp.watch(targets, function (event){
        testing(event);
    });
});


gulp.task('default', function (){
    gulp.run('all');
});
