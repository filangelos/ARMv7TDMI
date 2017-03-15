// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;

// require node modules before loader.js comes in
var path = require('path');
function uriFromPath(_path) {
  var pathName = path.resolve(_path).replace(/\\/g, '/');
  if (pathName.length > 0 && pathName.charAt(0) !== '/') {
    pathName = '/' + pathName;
  }
  return encodeURI('file://' + pathName);
}
amdRequire.config({
  baseUrl: uriFromPath(path.join(__dirname, '../node_modules/monaco-editor/min'))
});
// workaround monaco-css not understanding the environment
self.module = undefined;
// workaround monaco-typescript not understanding the environment
self.process.browser = true;
amdRequire(['vs/editor/editor.main'], function () {
  window.editor = monaco.editor.create(document.getElementById('editor'), {
    value: [
      'mov r0 #5',
      'mov r1 r0'
    ].join('\n'),
    //        language: 'arm',
    theme: 'vs-light',
    renderWhitespace: 'all'
  });
  const run_btn = document.getElementById('run')
  const btn = run_btn.onclick = function () {
    return window.editor.getValue()
  }
});

const message = "message variable";
const editorR = "window.editor"

function x(y) { console.log( y*y ); null;}

export { message, editorR, x, btn };