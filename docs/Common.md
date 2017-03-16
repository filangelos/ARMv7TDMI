# Common

## Types
* `Tokek` all the differnt types of tokens using for tokenisation
* `Data` type abbreviation of `int` representing the raw data
* `RegisterID` D.U `| R0 | R1 | ... | R15` representing the registers indexes
* `Registers` type abbreviation of `Map<RegisterID, Data>` representing the register index and content
* `FlagID` D.U `| N | V | C | Z` representing the status bits
* `Flags` type abbreviation of `Map<FlagID, bool>` representing the status bits and content
* Parameters = D.U of tuples for paramters
* Address = int
* type Node = Instruction * Parameters * Address
* type LabelMap = Map<string, Address>
* type AST = Node list
---
## Functions
* `Data` "cast" operator : int -> Data
* D.U type enumerator : 'T -> 'T [||]
---
## Dependencies