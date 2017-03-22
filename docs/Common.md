# Common

## Description

This module contains the interfaces shared between the various modules.

## Types
* `Token`: all the differnt types of tokens using for tokenisation
* `Data`: type abbreviation of `int` representing the raw data
* `RegisterID`: D.U. `| R0 | R1 | ... | R15` representing the registers indexes
* `Registers`: type abbreviation of `Map<RegisterID, Data>` representing the register index and content
* `FlagID`: D.U. `| N | V | C | Z` representing the status bits
* `Flags`: type abbreviation of `Map<FlagID, bool>` representing the status bits and content
* `ShiftDirection`: D.U. `Left of int | RightL of int | ... | RRX | NoShift` of different operand shifts within instruction 
* `Input`: D.U. `| ID of RegisterID | Literal of data` representing op2, which can be register or literal so 
* `Operand`: type abbreviation of `Input*ShiftDirection`
* `Offset`: D.U. `| TempOffset of Input | ... | PostIndex of Input | NoOffset` for memory offsetting 
* `AddressMode`: D.U. `| IA | IB | ... | EA | FA` representing addressing mode
* `StackDirection`: maps to the `AddressMode` D.U.
* `AddressRegister`: Record of `RegisterID*Offset`
* `Expression`: D.U. `| Lab of string | Number of int` for memory-related expressions
* `ConditionCode`: D.U. `| EQ | NE | CS | ...` representing the conditional codes in ARM instructions
* `InstrType1` ... `InstrType9`: represents the groups of instruction keywords with the same parameters
* `InstrDCD`, `InstrEQU`, `InstrFILL`, `InstrEND`: singular instruction keyword types
* `Instr`: = D.U. of tuples for different instructions and their parameters
* `Address`: = int, represent the byte addressing of the memory
* `Node`: = Instr * Address
* `LabelMap`: = Map<string, Address>
* `AST`: = Node list
---
## Functions
* `Data` "cast" operator : int -> Data
* D.U type enumerator : 'T -> 'T [||]
---
## Dependencies