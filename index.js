var r = require('react');
var d = require('react-dom');
var createReactClass = require('create-react-class');
var biwasyuni = require('./biwasyuni/biwasyuni_core.js');
var biwasloader = require('./loader_biwasyuni.js');

var root = document.body;

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

biwasyuni.switch_console_output(); // Use console.log
biwasyuni.add_module("e", e);
biwasyuni.add_module("createReactClass", createReactClass);
biwasyuni.add_module("fs", loadfs); // FIXME: ???
biwasyuni.add_module("js-load-async", js_load_async);
biwasyuni.add_module("document-root", root);
biwasyuni.set_current_fs(loadfs);

d.render(e("div", null, "Hello."), document.getElementById('root')); // debug

biwasyuni.run("(load \"boot.scm\")", 
              function(res){ console.log("init done.", res); },
              function(e){ throw e; });
