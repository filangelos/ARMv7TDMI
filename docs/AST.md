# AST

## Types

* type Parameters = D.U of tuples for paramters
* type Address = int
* type Condition = FlagID * bool
* type Node = InstructionKeyword * Parameters * (Condition option) * Address
* type LabelMap = Map<string, Address>
* type AST = (Node list) * LabelMap

---
## Functions

* function addInstruction: AST -> InstructionKeyword -> Parameters -> Condition option -> Address -> AST : adds an instruction node to the AST
* function addLabel: AST -> string -> Address -> AST : adds a label to the AST  with the specified address
* function reduce: AST -> MachineState -> MachineState : executes instructions in an AST and returns the final MachineState
---
## Dependencies

Common.fs
MachineState.fs
Instructions.fs