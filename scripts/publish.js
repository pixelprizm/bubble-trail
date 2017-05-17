// This file doesn't build, it just publishes.
var ghpages = require('gh-pages');
var paths = require('../config/paths');

ghpages.publish(paths.dist);
