"use strict";

var path = require("path");

var roots = [".", ".."].map(e => path.resolve(__dirname, e));

var config = {
  getPlatforms() {
    return ["dom", "windows"];
  },
  getProvidesModuleNodeModules() {
    return ["react-native", "react-native-dom", "react-native-windows"];
  },
  // FIXME: Resent RN commit introduced getAssetExt()
  getAssetExts(){
      return ["scm", "sps", "sls" ];
  },
  getProjectRoots(){
      console.log(roots);
      return roots;
  }
};


module.exports = config;
