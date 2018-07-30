var m = require('./node_modules/mithril');
var biwasyuni = require('./biwasyuni/biwasyuni_core.js');
var biwasloader = require('./loader_biwasyuni.js');
var bfs = require('./node_modules/browserfs');
require('./node_modules/bootstrap');

var root = document.body;

var async_loaders = {
    prosemirror: import('./loader_prosemirror.js')
};

var js_load_async = function(name, cb){
    async_loaders[name].then(cb);
};

var loadfs = biwasloader.loadfs("/");

biwasyuni.switch_console_output(); // Use console.log
biwasyuni.add_module("m", m);
biwasyuni.add_module("browserfs", bfs);
biwasyuni.add_module("fs", loadfs); // FIXME: ???
biwasyuni.add_module("js-load-async", js_load_async);
biwasyuni.add_module("document-root", root);
biwasyuni.set_current_fs(loadfs);

m.render(root, "Hello."); // debug

biwasyuni.run("(load \"boot.scm\")", 
              function(res){ throw res; },
              function(e){ throw e; });
