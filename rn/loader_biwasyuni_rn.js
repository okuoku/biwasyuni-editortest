var assets = require("./gen/assets.js").assets;
var im = require("Image");

var loadfs = function(){
    var me = {
        readFile: function(path, bogus_coding, callback) {
            // FIXME: Implement JSON archive case
            var asset = assets[path];
            if(! asset.FIXMEFIXMEFIXME){ /* Debugging */
                var uri = im.resolveAssetSource(asset);
                fetch(uri).then(res => {
                    if(res.ok){
                        res.text().then(text => callback(false, text));
                    }else{
                        callback("Error", false);
                    }
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
