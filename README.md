Decomposition
=============

## Modules

* Program [ ] - 
* Assembler [ ] - _Youssef & Baron_
    * Tokenizer [ ] - _Baron & Pranav_
    * Parser [ ] - _Baron & Pranav_
    * AST [ ] - 
* Emulator [ ] - _Angelos & Youssef & Baron_
    * Arithmetic Instructions [ ] - 
    * Load/Store Instructions [ ] -
    * Branch Instructions [ ] - 
    * Other Instructions [ ] - 
* Memory [ ] - _Angelos_
* Front-End [ ] - _Angelos_


## Project Milestones

- [x] Figure out what we're supposed to do
- [x] Figure out how we're supposed to start
- [ ] Write ADC function
- [ ] Come up with machineState interface
- [ ] Write basic tokeniser
- [ ] Write basic parser
- [ ] Write basic AST


## Functions and Types Exported from each Module

### Common
(NB: we will need to combine the Common modules from each solution)
* type Token
* type Data
* type RegisterID
* type Registers
* type FlagID
* type Flags

### MachineState
* type MachineState
* function make: unit

### Optics
* type Lens<'a, 'b>
* inline function get: 'a * 'b -> 'a
* inline function set: 'a * 'b -> 'b

### Assembler
* function readAsm: string -> MachineState

### Tokeniser
* function tokenise: string -> Token list
* function tokeniseTest: unit

### Parser
* 

