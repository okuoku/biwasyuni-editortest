var m = require('./node_modules/mithril');
var biwasyuni = require('./biwasyuni/biwasyuni_core.js');
var biwasloader = require('./loader_biwasyuni.js');
var bfs = require('./node_modules/browserfs');

var root = document.body;

var load_prosemirror = function(recv){
    import('./loader_prosemirror.js').then(function(lib) {
        recv(lib);
    });
};

var loadfs = biwasloader.loadfs("/");

biwasyuni.switch_console_output(); // Use console.log
biwasyuni.add_module("m", m);
biwasyuni.add_module("browserfs", bfs);
biwasyuni.add_module("fs", loadfs); // FIXME: ???
biwasyuni.set_current_fs(loadfs);

m.render(root, "Hello."); // debug

biwasyuni.run("(load \"boot.scm\")", 
              function(res){ throw res; },
              function(e){ throw e; });
