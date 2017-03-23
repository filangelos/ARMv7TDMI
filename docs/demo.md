# Electron Demo
The application is developed according to the MVU Elmish Architecture in F# and transpiled to JS using Fable Compiler. 
Github Electron is used for running the app as a native desktop application.
`Monaco Editor` is used for the text input and `PhotonKit` as the main CSS package.

## Files
* `./ElectronMain.js` electron renderer JS file.
* `./dist/index.html` main HTML file.
* `./src/fs/View.fs` access to the DOM element.
* `./src/fs/Update.fs` actions triggered by user inputs.
* `./src/fs/Parser.fs` alternative parser used due to incompatibility issues [Fable Issue #756](https://github.com/fable-compiler/Fable/issues/756).
* `./src/js/Editor.js` configuration of `Monaco Editor`.
---
## Requirements
* __node.js__.
* electron github, globally installed.
---
## Installation

In the root of the project run `npm install` or `yarn` in the CLI.
When the necessary `node_modules` are installed, run `npm/yarn start`.

---
## Milestones
- [x] Integrate Monaco Editor
- [x] Split View - Editor and Register Values
- [x] Highlighting
- [x] Open files
- [ ] Save files
- [x] Theme changing
- [x] Indent Text
- [x] Debug Mode
- [ ] Error Handling
---
## Contributors
Angelos Filos
