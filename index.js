var r = require('react');
var d = require('react-dom');
var createReactClass = require('create-react-class');
var biwasyuni = require('./biwasyuni/biwasyuni_core.js');
var biwasloader = require('./loader_biwasyuni.js');

var root = document.getElementById('root');

var async_loaders = {
    prosemirror: import('./loader_prosemirror.js'),
    browserfs: import('./node_modules/browserfs'),
    materialui: import('@material-ui/core/umd/material-ui.production.min.js')
};

var js_load_async = function(name, cb){
    async_loaders[name].then(cb);
};

var loadfs = biwasloader.loadfs("/");

var e = r.createElement;

var thiswrap = function(cb){
    return function(){
        return cb(this);
    };
};

var pp = function(a){
    console.log(a);
};

biwasyuni.switch_console_output(); // Use console.log
biwasyuni.add_module("e", e);
biwasyuni.add_module("d", d);
biwasyuni.add_module("createReactClass", createReactClass);
biwasyuni.add_module("fs", loadfs); // FIXME: ???
biwasyuni.add_module("js-load-async", js_load_async);
biwasyuni.add_module("document-root", root);
biwasyuni.add_module("thiswrap", thiswrap);
biwasyuni.add_module("ReactFragment", r.Fragment);
biwasyuni.add_module("pp", pp);
biwasyuni.set_current_fs(loadfs);

d.render(e("div", null, "Starting yuni..."), root); // debug

biwasyuni.run("(load \"boot.scm\")", 
              function(res){ console.log("init done.", res); },
              function(e){ throw e; });
