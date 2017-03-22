# Instructions

## Description

This module contains the fundamental functions that execute ARM ALU instructions. Contained here are generic Add, Subtract, Mov, and Mvn instructions which are used as the building blocks in the implementation of the actual ARM instructions, as well as bitwise logical instructions such as ORR, AND, BIC, and EOR

---
## Types
* No types defined here
---
## Functions

* `opVal: MachineState -> Input -> Data`: Extracts the data (int) contained in the operand op2
* `getOperandVal: bool -> MachineState -> Operand -> Data*MachineState`: Applies a shift to the operand op2 and sets the corresponding flags if necessary
* `mov: RegisterID * Operand * MachineState * bool -> MachineState`: Generic mov instruction. Can set NZC flags if specified and is used as the underlying instruction for shift/rotation instructions
* `mvn: RegisterID * Operand * MachineState * bool -> MachineState`: mvn instructions. Can set NZC flags if specified
* `addWithCarryS: RegisterID * RegisterID * Operand * MachineState * bool * bool -> MachineState`: Generic add function. Can set NZCV flags if specified and is used as the underlying instruction for ADD{S}, ADC{S}, and CMN
* `orr: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState`: Bitwise ORR instruction. Can set NZ flags if specified
* `andOp: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState`: Bitwise AND instruction. Can set NZ flag if specified
* `eOR: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState`: Bitwise EOR instruction. Can set NZ flag if specified
* `bic: RegisterID * RegisterID * Operand * MachineState * bool -> MachineState`: Bitwise BIC instruction. Can set NZ flag if specified
* `subtractWithCarryS: RegisterID * Operand * Operand * MachineState * bool * bool -> MachineState`: Generic subtract function. Can set NZCV flags if specified and is used as the underlying instruction for SUB{S}, SBC{S}, RSB{S}, RSC{S}, CMP,

---
## Dependencies

* Common.fs
* MachineState.fs
* Optics.fs

## Notes

* These functions only directly affect the `InstructionsInterfaces` module as they are used to make up the ARM instruction set.
* Conceptually, the instructions are implemented as a pipeline starting with `opVal |> getOperandVal |> instruction` where `instruction` is any of the actual ALU instructions.
* Minor testing is present at the end of the code as functions of type `unit -> MachineState`, which can be called from Program.fs. More comprehensive testing is done in the testing project which interfaces with VisUAL

## Owner: Youssef Rizk
