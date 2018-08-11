var biwasyuni = require("../biwasyuni/biwasyuni_core.js");
var biwasloader = require("./loader_biwasyuni_rn.js");
var r = require("react");
var createReactClass = require("create-react-class");
var loadfs = biwasloader.loadfs();

var e = r.createElement;

var thiswrap = function(cb){
    return function(){
        return cb(this);
    };
};

function startup(lib, cb){
    biwasyuni.switch_console_output();
    biwasyuni.add_module("fs", loadfs);
    biwasyuni.add_module("rn-libs", lib);
    biwasyuni.add_module("e", e);
    biwasyuni.add_module("createReactClass", createReactClass);
    biwasyuni.add_module("thiswrap", thiswrap);
    biwasyuni.set_current_fs(loadfs);

    biwasyuni.run("(load \"boot.scm\")",
                  function(res){ 
                      console.log("init done.", res);
                      cb(res);},
                  function(e){ throw e; });
}

module.exports = {startup:startup};

