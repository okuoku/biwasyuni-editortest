//var approot_url = "/release"; // NB: Override on debug build
var approot_url = "";

var bd =require('../node_modules/parcel');
var express  = require('../node_modules/express');
var Path = require('path');
var fs = require('fs');
var fse = require('fs-extra');
var buildtype = process.argv[2];

var writeconfig = function(pth){
    var cfg = {
        approot: approot_url
    };
    var out = "module.exports = " + JSON.stringify(cfg) + ";\n";
    fs.writeFileSync(pth, out);
};

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

// Application provider
var appprovider = function(lst){
    // lst = [ LIB* ]
    // LIB = {libname: #f/[name*], dir: DIR, pth: PATH}

    // Add "app.sps" as an entrypoint
    lst.push({libname: false, dir: ".", pth: "app.sps"});

    if(buildtype == "debug" || buildtype == "debug_minify"){
        const input = Path.join(__dirname, "../index_debug.html");
        // Override approot_url
        approot_url = "";

        // generate bootloader
        if(! fs.existsSync("dist")){
            fs.mkdirSync("dist");
        }
        genboot("dist/boot.scm", lst);
        writeconfig("dist/appconfig.js");

        // It seems detailedReport requires !watch
        var options = { 
            outFile: "index.html",
            watch: false, 
            detailedReport: true 
        };
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

        // Construct file => dir mapping
        var dirmap = {};
        lst.forEach(e => {dirmap[e.pth] = e.dir;});

        // Static provider
        const approot = Path.join(__dirname, "..");
        app.get('*', function(req, res, next){
            dir = dirmap[req.url];
            if(dir){
                var sendpath = approot + "/" + dir + "/" + req.url;
                console.log("Send", sendpath);
                res.setHeader("Content-Type", "text/plain");
                res.sendFile(sendpath);
            }else{
                console.log("Not found", req.url);
                next();
            }
        });

        app.listen(8080);
    }else if(buildtype == "release"){
        const input = Path.join(__dirname, "../index_release.html");
        // generate bootloader
        if(! fs.existsSync("release")){
            fs.mkdirSync("release");
        }
        genboot("release/boot.scm", lst);
        writeconfig("release/appconfig.js");

        // Copy asset files to the destination
        lst.forEach(e => {
            var source = e.dir + "/" + e.pth;
            var dest = e.pth;
            fse.copySync(source, "release/" + dest);
        });
        // Bundle
        var options = { watch: false, detailedReport: true,
            outFile: "index.html",
            outDir: "./release",
            minify: true,
            scopeHoist: false,
            sourceMaps: false,
            publicUrl: approot_url

        };
        var bundler = new bd(input, options);
        bundler.bundle();
    }else{
        throw "Unknown build type";
    }
};

// Generate bootstrap filelist
var by = require("../biwasyuni/biwasyuni_node.js");
by.gen_filelist("yuni", ["yunilib"], ["app.sps"], appprovider);
