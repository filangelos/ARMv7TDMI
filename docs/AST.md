# AST

## Types



---
## Functions
* function private addInstruction: AST -> Instr -> Address -> AST : adds an instruction node to the AST
* function private addLabel: Map<string,Address> -> string -> Address -> Map<string,Address> : adds a label to the label map
* function buildMemory: Instr list -> AST * Map<string,Address> *  : builds an AST and label map from a parse list
* function step: MachineState -> MachineState : executes the instruction in the AST with the address stored in R15
* function reduce: MachineState -> int -> int -> MachineState : executes all instructions between a start and end address in an AST starting from pc upt to maxPC
* function execute: MachineState -> MachineState : executes the entire AST of a MachineState
---
## Dependencies

Common.fs
MachineState.fs
InstructionInterfaces.fs
Memory.fs