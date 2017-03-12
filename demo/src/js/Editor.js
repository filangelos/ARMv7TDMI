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
  run_btn.onclick = function () {
    message = window.editor.getValue()
    console.log(message);
  }
});

export var message = "message variable";
export var editorR = "window.editor"