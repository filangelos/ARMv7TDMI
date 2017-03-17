# Instructions

## Types
*
---
## Functions

* function sortRegister: RegisterID list -> RegisterID list
* function addressWithOffset: MachineState -> AddressRegister -> Data
* function loadInstructionW: RegisterID * AddressRegister * MachineState -> MachineState
* function storeInstructionW: Data * AddressRegister * MachineState -> MachineState
* function loadInstructionB: RegisterID * AddressRegister * MachineState -> MachineState
* function storeInstructionB: Data * AddressRegister * MachineState -> MachineState
* function loadMultiple: AddressMode * RegisterID * RegisterID list * MachineState * bool
* function storeMultiple: AddressMode * RegisterID * RegisterID list * MachineState * bool
* function ldrPseudo: RegisterID * Expression * MachineState -> MachineState
* function fillInstruction: string * int * MachineState -> MachineState
* function dcdInstruction: string * int list * MachineState -> MachineState
* function branchInstruction: string * MachineState -> MachineState

---
## Dependencies

* Common.fs
* Instructions.fs
* MachineState.fs
* Optics.fs

## Notes


