var assets = require("./gen/assets.js").assets;
var im = require("Image");

var loadfs = function(){
    var me = {
        readFile: function(path, bogus_coding, callback) {
            // FIXME: Implement JSON archive case
            var asset = assets[path];
            if(! asset.FIXMEFIXMEFIXME){ /* Debugging */
                var uri = im.resolveAssetSource(asset).uri;
                console.log("Loading ", uri);
                fetch(uri).then(res => {
                    // FIXME: Error handling..?
                    res.text().then(function(text){
                        callback(false, text);
                    });
                });
            }else{
                console.log("loader_biwasyuni: no uri? ????");
            }
        }
    };
    return me;
};

module.exports = {
    loadfs:loadfs
}
