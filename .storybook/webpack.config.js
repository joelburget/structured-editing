var path = require('path');
var genDefaultConfig = require('@kadira/storybook/dist/server/config/defaults/webpack.config.js');
var CircularDependencyPlugin = require('circular-dependency-plugin');

module.exports = function(config, env) {
  var config = genDefaultConfig(config, env);

  config.devtool = 'source-map';
  // config.plugins = [
  //   new CircularDependencyPlugin({
  //     exclude: /node_modules/,
  //     failOnError: true,
  //   })
  // ];

  return config;
};
