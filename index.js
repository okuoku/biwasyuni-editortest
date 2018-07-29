var m = require('./node_modules/mithril');
var biwasyuni = require('./biwasyuni/biwasyuni_core.js');
var bfs = require('./node_modules/browserfs');

var root = document.body;

var load_prosemirror = function(recv){
    import('./loader_prosemirror.js').then(function(lib) {
        recv(lib);
    });
};

m.render(root, "Hello."); // debug

