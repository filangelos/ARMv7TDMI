# AST

## Types



---
## Functions
* function addInstruction: AST -> Instruction -> Parameters -> Address -> AST : adds an instruction node to the AST
* function addLabel: AST -> string -> Address -> AST : adds a label to the AST  with the specified address
* function buildAST: (Instruction * Parameters) list -> AST
* function step: MachineState -> MachineState : executes the instruction in the AST with the address stored in R15
* function reduce: MachineState -> MachineState : executes instructions in an AST starting from pc = 0
---
## Dependencies

Common.fs
MachineState.fs
Instructions.fs