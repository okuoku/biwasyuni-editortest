"use strict";

var path = require("path");

var roots = [".", ".."].map(e => path.resolve(__dirname, e));

var config = {
  getPlatforms() {
    return ["dom"];
  },
  getProvidesModuleNodeModules() {
    return ["react-native", "react-native-dom"];
  },
  // FIXME: Resent RN commit introduced getAssetExt()
  getAssetExts(){
      return ["scm", "sps", "sls" ];
  },
  getProjectRoots(){
      return roots;
  }
};


module.exports = config;
