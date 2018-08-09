var by = require("../../biwasyuni/biwasyuni_node.js");
var fs = require("fs");

var genboot = function(pth, lst){
    var out = "(define (command-line) '(\"\" \"\" \"\" \"\"))\n";
    lst.forEach(e => { 
        out += "(load \"" + e.pth + "\")\n"; 
        if(e.alias){
            var fromname = e.libname.join(" ");
            var toname = e.alias.join(" ");
            out += "(yuni/register-library-alias! '(" + fromname + 
                ") '(" + toname + "))\n";
        }
    });
    fs.writeFileSync(pth, out);
};

function genlist(lst){
    lst.push({libname: false, dir: "..", pth: "app.sps"});

    if(! fs.existsSync("gen")){
        fs.mkdirSync("gen");
    }

    genboot("gen/boot.scm", lst);

    var out = "var assets = {\n";
    lst.forEach(e => {
        // As we place assets.js under gen/ we need an additional ../
        out += "    \"" + e.pth + "\": require(\"../" + e.dir + "/" +
            e.pth + "\"),\n";
    });

    out += "    \"boot.scm\": require(\"../gen/boot.scm\")\n";

    out += "}\nmodule.exports = { assets:assets };";

    fs.writeFileSync("gen/assets.js", out);
}

by.gen_filelist("../yuni", ["../yunilib"], ["../app.sps"], genlist);
