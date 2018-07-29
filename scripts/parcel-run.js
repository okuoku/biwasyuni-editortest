var bd = require('../node_modules/parcel');
var express  = require('../node_modules/express');
var Path = require('path');

// Application provider
var appprovider = function(bogus){
    const input = Path.join(__dirname, "../index.html");

    var app = express();
    var options = { };
    var bundler = new bd(input, options);
    // Root
    app.get("/", function(req, res){ res.redirect("/index.html")});

    // Static provider
    const yuniroot = Path.join(__dirname, "../yuni");
    var serve_yuniroot = express.static(yuniroot);
    app.use("/yuni", serve_yuniroot);
    console.log(yuniroot);

    // Parcel application
    app.use(bundler.middleware());

    app.listen(8080);
};


// Run preroll script
var biwasyuni = require('../biwasyuni/biwasyuni_core.js');
var fs = require('fs');
var preroll = fs.readFileSync(__dirname + "/preroll.scm", 'utf8');
var biwaserror = function(e){
    console.error(e.stack ? e.stack : e.toString ? e.toString() : e);
};
biwasyuni.add_module("fs", fs);
biwasyuni.set_current_fs(fs);

biwasyuni.run(preroll, appprovider, biwaserror);
