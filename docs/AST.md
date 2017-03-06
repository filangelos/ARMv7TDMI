# AST

## Types

* type Parameters = D.U of tuples for paramters
* type Address = int
* type Node = DU of InstructionNode | LabelNode | NullNode
* type LabelMap = Map<string, Address>
* type AST = (Node list) * LabelMap

---
## Functions

* function inline addInstructionNode: AST -> InstructionKeyword -> Parameters -> Address) -> AST : adds an instruction node to the AST
* function inline addLabelNode: AST -> string -> Address -> AST : adds a label node to the AST
* function reduce: AST -> machineState -> MachineState : executes the AST
---
## Dependencies

Common.fs
MachineState.fs
Instructions.fs