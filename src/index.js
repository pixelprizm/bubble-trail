require('./main.css');
var Elm = require('./Main.elm');

// Set up e.preventDefault() on Tab
// copied from: https://github.com/ohanhi/keyboard-extra/issues/23#issuecomment-304420728
document.documentElement.addEventListener('keydown', function (e) {
    if (e.which == 9) { // 9 is tab
        e.preventDefault();
    }
}, false);

var root = document.getElementById('root');

Elm.Main.embed(root);
