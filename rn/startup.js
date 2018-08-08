var biwasyuni = require("../biwasyuni/biwasyuni_core.js");
var biwasloader = require("./loader_biwasyuni_rn.js");

var loadfs = biwasloader.loadfs();

function startup(){
    biwasyuni.switch_console_output();
    biwasyuni.add_module("fs", loadfs);
    biwasyuni.set_current_fs(loadfs);

    biwasyuni.run("(load \"boot.scm\")",
                  function(res){ console.log("init done.", res);},
                  function(e){ throw e; });
}

module.exports = {startup:startup};

