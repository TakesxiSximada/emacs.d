// -*- coding: utf-8 -*-
var Dropsaws = {
    REVISION: '0.1.0',
    // logger
    log: function () {console.log.apply(console, arguments);},
    warn: function () {console.warn.apply(console, arguments);},
    error: function () {console.error.apply(console, arguments);},
};

// browserify support
if (typeof module === 'object'){
    module.exports = Dropsaws;
}

// polyfills
if (Math.sign === undefined){
    Math.sign = function (x){
        return (x < 0) ? -1 : (x > 0) ? 1 : +x;
    }
}

Dropsaws.MainView = function (window, selector){
    this.window = window;
    this.selector = selector;
    this._core = null;
};
Dropsaws.MainView.prototype = {
    // documentオブジェクト
    get document (){
        return this.window.document;
    },
    // 画面幅
    get width (){
        return this.window.innerWidth;
    },
    // 画面高さ
    get height (){
        return this.window.innerHeight;
    },
    // アスペクト比
    get aspect (){
        return this.width / this.height;
    },
    // なんだこれ?
    get devicePixelRatio (){
        return this.window.devicePixcelRatio;
    },
    update_seeds: function (selector){
        var elm = this.document.querySelector(selector);
        var seeds = this._core.$data['seeds']
        elm.addEventListener('blur', function (event){
            var url = event.target.value;
            var parser = new URL(url);
            var hostname = parser.hostname;
            hostname.split('.').forEach(function (element, index, array){
                if (element != '' && seeds.indexOf(element) == -1){
                    seeds.push(element);
                    seeds.sort();
                }
            });
        });
    },
    view: function (){
        this._core = new Vue({
            el: this.selector,
            data: {
                email: '',
                password: '',
                seeds: [],
                char_count: [
                    {count: 8},
                    {count: 9},
                    {count: 10},
                    {count: 11},
                    {count: 12},
                    {count: 13},
                ],
            },
            font: '',
            methods: {
                copy_word: function (item){
                    var count = item.$data.count;
                    var seed = item.$parent.$value;
                    var email = item.$root.$data.email;
                    var password = item.$root.$data.password;
                    var word = get_password(seed, email, password, count);
                    alert(word);
                },
            },
        });
    },
};

function bin2char(num, solt){
    solt = solt % 5;
    while(!((48 <= num && num <= 57) ||
	        (65 <= num && num <= 90) ||
	        (97 <= num && num <= 122))){
	    num = (48 + num + solt) % 123;
    }
    return String.fromCharCode(num)
}

function do_hash(text){
    return sha256.dec(text);
}

function get_password(word0, word1, solt, length){
    var before_num = 3;
    var ret_word = '';
    var line = word1 + word0 + solt;
    var _nums = do_hash(line);
    for(var ii = 0; ii < length; ii++){
	    var _num = _nums[ii];
	    var _chr = bin2char(_num, before_num);
	    before_num = _num;
	    ret_word = ret_word + _chr;
    }
    return ret_word;
}
