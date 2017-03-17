# Instructions

## Types
*
---
## Functions

* function executeOrNot: ConditionCode option -> MachineState -> bool
* function mov_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function mvn_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function add_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function adc_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function sub_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function sbc_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function rsb_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function rsc_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function cmp_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState
* function cmn_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState
* function tst_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState
* function teq_: RegisterID * Operand * MachineState * ConditionCode option -> MachineState
* function ror_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function rrx_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function lsl_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function lsr_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function asr_: RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function orr_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option -> MachineState
* function and_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option  -> MachineState
* function eor_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option  -> MachineState
* function bic_: RegisterID * RegisterID * Operand * MachineState * bool * ConditionCode option  -> MachineState
* function ldr_: RegisterID * AddressRegister * MachineState * ConditionCode option -> MachineState
* function ldrb_: RegisterID * AddressRegister * MachineState * ConditionCode option -> MachineState
* function str_: Data * AddressRegister * MachineState * ConditionCode option -> MachineState
* function strb_: Data * AddressRegister * MachineState * ConditionCode option -> MachineState
* function ldm_: AddressMode * RegisterID * RegisterID list * MachineState * writeBack * ConditionCode option -> MachineState
* function stm_: AddressMode * RegisterID * RegisterID list * MachineState * writeBack * ConditionCode option -> MachineState
* function psuedo_ldr_: RegisterID * Expression * MachineState * ConditionCode option -> MachineState
* function adr_: RegisterID * Expression * MachineState * ConditionCode option -> MachineState

---
## Dependencies

* Common.fs
* Instructions.fs
* MemoryInstructions.fs
* MachineState.fs
* Optics.fs

## Notes

There are the final interfaces that other modules interact with.
