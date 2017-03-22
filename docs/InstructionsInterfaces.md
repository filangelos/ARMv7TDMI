# Instructions

## Description

This module serves the primary interface between the execution part of the project and all other modules. It implements each of the VisUAL supported instructions using the functions defined in `Instructions` and `MemoryInstructions` modules. This implementation follows the Computer Architecture module notes.

---
## Types
* No types defined here
---
## Functions

* `executeOrNot: ConditionCode option -> MachineState -> bool`: Determines whether a specific function should be implemented or not depending on the condition codes and the current state of the machine.
* `mov_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements MOV{S}{cond} instruction
* `mvn_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements MVN{S}{cond} instruction
* `add_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements ADD{S}{cond} instruction
* `adc_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements ADC{S}{cond} instruction
* `sub_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements SUB{S}{cond} instruction
* `sbc_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements SBC{S}{cond} instruction
* `rsb_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements RSB{S}{cond} instruction
* `rsc_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements RSC{S}{cond} instruction
* `cmp_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState`: Implements CMP{cond} instruction
* `cmn_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState`: Implements CMN{cond} instruction
* `tst_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState`: Implements TST{cond} instruction
* `teq_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState`: Implements TEQ{cond} instruction
* `ror_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements ROR{S}{cond} instruction
* `rrx_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements RRX{S}{cond} instruction
* `lsl_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements LSL{S}{cond} instruction
* `lsr_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements LSR{S}{cond} instruction
* `asr_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements ASR{S}{cond} instruction
* `orr_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState`: Implements ORR{S}{cond} instruction
* `and_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option  -> MachineState`: Implements AND{S}{cond} instruction
* `eor_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option  -> MachineState`: Implements EOR{S}{cond} instruction
* `bic_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option  -> MachineState`: Implements BIC{S}{cond} instruction
* `ldr_: RegisterID * AddressRegister * MachineState * ConditionCode option -> MachineState`: Implements LDR{cond} instruction
* `ldrb_: RegisterID * AddressRegister * MachineState * ConditionCode option -> MachineState`: Implements LDRB{cond} instruction
* `str_: Data * AddressRegister * MachineState * ConditionCode option -> MachineState`: Implements STR{cond} instruction
* `strb_: Data * AddressRegister * MachineState * ConditionCode option -> MachineState`: Implements STRB{cond} instruction
* `ldm_: AddressMode * RegisterID * RegisterID list * MachineState * writeBack * ConditionCode option -> MachineState`: Implements LDM{add_mode}{cond} instruction
* `stm_: AddressMode * RegisterID * RegisterID list * MachineState * writeBack * ConditionCode option -> MachineState`: Implements STM{add_mode}{cond} instruction
* `psuedo_ldr_: RegisterID * Expression * MachineState * ConditionCode option -> MachineState`: Implements Psuedo LDR{cond} Instruction
* `adr_: RegisterID * Expression * MachineState * ConditionCode option -> MachineState`: Implements ADR{cond} instruction
* `b_: string * MachineState * ConditionCode option`: Implements B{cond} Instruction

---
## Dependencies

* Common.fs
* Instructions.fs
* MemoryInstructions.fs
* MachineState.fs
* Optics.fs

## Notes

* This module abstracts the implementation of the execution part from any other module in the project.
* DCD and FILL are dealt with by the AST module since they define start-time conditions.
