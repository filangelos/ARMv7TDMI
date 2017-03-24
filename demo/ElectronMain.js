const electron = require('electron')
// Module to control application life.

// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow

const client = require('electron-connect').client
// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
var mainWindow = null;

// demonstrate how to load and modify a menu bar
// this menu can be found in the electron-default-menu Module
// you could extract the menu template, modify its source, and use it directly
// but in this example we will modify it programmatically
const defaultMenu = require('electron-default-menu');
const { Menu, app, shell } = electron;
const dialog = require('dialog');


// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform != 'darwin') {
    app.quit();
  }
});

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', function () {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    frame: true,
    titleBarStyle: 'hidden',
    resizable: false
  });

  console.log('ElectronMain.js has created the main window')
  // and load the index.html of the app.
  mainWindow.loadURL('file://' + __dirname + '/dist/index.html');
  console.log('ElectronMain.js has loaded dist/index.html')

  // Open Development Tools
//  mainWindow.openDevTools();

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    console.log('ElectronMain is dereferencing the main window.')
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null;
  });
});

