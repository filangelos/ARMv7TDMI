# Instructions

## Description

This module contains the execution code for the ARM memory access instructions. The current VisUAL instruction set is supported by these functions. Again, these are underlying implementations and are directly used by `InstructionsInterfaces` module.

---
## Types
* No types defined here
---
## Functions

* `sortRegister: RegisterID list -> RegisterID list`: Implements a sorting mechanism to sort `RegisterID list`. 
* `addressWithOffset: MachineState -> AddressRegister -> Data`: Extracts the true address that will be used for the load/store operation depending on the offset associated with the oepration
* `loadInstructionW: RegisterID * AddressRegister * MachineState -> MachineState`: Implements LDR instruction (word).
* `storeInstructionW: Data * AddressRegister * MachineState -> MachineState`: Implements STR instruction (word).
* `loadInstructionB: RegisterID * AddressRegister * MachineState -> MachineState`: Implements LDR instruction (byte).
* `storeInstructionB: Data * AddressRegister * MachineState -> MachineState`: Implements STR instruction (byte).
* `loadMultiple: AddressMode * RegisterID * RegisterID list * MachineState * bool`: Implements LDM instruction. Can accept addressing mode suffixes as well as stack-oriented suffixes.
* `storeMultiple: AddressMode * RegisterID * RegisterID list * MachineState * bool`: Implements STM instruction. Can accept addressing mode suffixes as well as stack-oriented suffixes.
* `ldrPseudo: RegisterID * Expression * MachineState -> MachineState`: Implements psuedo LDR instruction. Also used as underyling implementation for ADR
* `branchInstruction: string * MachineState -> MachineState`: Implements branching instructions.

---
## Dependencies

* Common.fs
* Instructions.fs
* MachineState.fs
* Optics.fs

## Notes

* As stated in the Functions section, the LDM/STM instructions support the two types of addressing modes, while VisUAL only supports stack-oriented ones.
* `sortRegister` is created to accomodate for FABLE, since passing `RegisterID list` to List.sort works in F# but does not work with FABLE, and hence the suboptimal implementation.
* This module is again only used by `InstructionsInterfaces` module.

## Owner: Youssef Rizk