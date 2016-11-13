var path = require('path');
var genDefaultConfig = require('@kadira/storybook/dist/server/config/defaults/webpack.config.js');

module.exports = function(config, env) {
  var config = genDefaultConfig(config, env);

  config.devtool = 'source-map';

  return config;
};
