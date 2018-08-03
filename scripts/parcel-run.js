var bd = require('../node_modules/parcel');
var express  = require('../node_modules/express');
var Path = require('path');
var fs = require('fs');
var fse = require('fs-extra');
var buildtype = process.argv[2];

// Application provider
var appprovider = function(lst){
    const input = Path.join(__dirname, "../index.html");

    if(buildtype == "debug" || buildtype == "debug_minify"){
        // It seems detailedReport requires !watch
        var options = { watch: false, detailedReport: true };
        if(buildtype == "debug_minify"){
            options["minify"] = true;
            options["sourceMaps"] = false;
        }
        var bundler = new bd(input, options);

        var app = express();
        // Root
        app.get("/", function(req, res){ res.redirect("/index.html")});

        // Parcel application
        app.use(bundler.middleware());

        // Static provider
        const approot = Path.join(__dirname, "..");
        var serve_approot = express.static(approot);
        app.use("/", serve_approot);

        app.listen(8080);
    }else if(buildtype == "release"){
        // Copy asset files to the destination
        lst.forEach(e => {
            fse.copySync(e, "release/" + e);
        });
        // Copy bootloader
        fse.copySync("dist/boot.scm", "release/boot.scm");
        // Bundle
        var options = { watch: false, detailedReport: true,
            outDir: "./release",
            minify: true,
            scopeHoist: false,
            sourceMaps: false
        };
        var bundler = new bd(input, options);
        bundler.bundle();
    }else{
        throw "Unknown build type";
    }
};


// Run preroll script
var biwasyuni = require('../biwasyuni/biwasyuni_core.js');
var preroll = fs.readFileSync(__dirname + "/preroll.scm", 'utf8');
var biwaserror = function(e){
    console.error(e.stack ? e.stack : e.toString ? e.toString() : e);
};
biwasyuni.add_module("fs", fs);
biwasyuni.set_current_fs(fs);

biwasyuni.run(preroll, appprovider, biwaserror);
