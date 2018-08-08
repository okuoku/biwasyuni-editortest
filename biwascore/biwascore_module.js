var BiwaScheme = require("./gen/biwascore.js");

var extra_lib = require("./gen/extra_lib.js");
var js_interface = require("./gen/js_interface.js");
var r6rs_lib = require("./gen/r6rs_lib.js");
var srfi = require("./gen/srfi.js");

// Register standard libraries
extra_lib(BiwaScheme);
js_interface(BiwaScheme);
r6rs_lib(BiwaScheme);
srfi(BiwaScheme);

// Node libraries
BiwaScheme.activate_node_functions = function(fs,path,process){
    var node_functions = require("./gen/node_functions.js");
    BiwaScheme.on_node = true;
    BiwaScheme.node_apis = {
        fs: fs,
        path: path,
        process: process
    };
    node_functions(BiwaScheme);
}

module.exports = BiwaScheme;
