# Instructions

## Types
*
---
## Functions

* function `opVal: MachineState -> Input -> Data`
* function getOperandVal: bool -> MachineState -> Operand -> Data*MachineState
* function mov: RegisterID * Operand * MachineState * bool -> MachineState
* function mvn: RegisterID * Operand * MachineState * bool -> MachineState
* function addWithCarryS: RegisterID * RegisterID * Operand * MachineState * bool * bool -> MachineState
* function orr: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState
* function andOp: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState
* function eOR: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState
* function bic: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState
* function subtractWithCarryS: RegisterID * Operand * Operand * MachineState * bool * bool -> MachineState

---
## Dependencies

* Common.fs
* MachineState.fs
* Optics.fs

## Notes

These functions can implement more than one of the instructions, so these are not the final instructions that the tokenizer will deal with

