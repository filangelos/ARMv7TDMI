MachineState
============

## Dependencies



## Types and Functions per File

### Optics

#### Types
* `Lens<'a, 'b>` tuple of a getter and a setter

#### Functions
* `inline function get: 'a * 'b -> 'a` getter function
* `inline function set: 'a * 'b -> 'b` *immutable* setter, returns a new `'b` with the updated entry

### Common

#### Types
* `Data` type abbreviation of `int` representing the raw data
* `RegisterID` D.U `| R0 | R1 | ... | R15` representing the registers indexes
* `Registers` type abbreviation of `Map<RegisterID, Data>` representing the register index and content
* `FlagID` D.U `| N | V | C | Z` representing the status bits
* `Flags` type abbreviation of `Map<FlagID, bool>` representing the status bits and content

#### Functions
* `Data` "cast" operator : int -> Data
* D.U type enumerator : 'T -> 'T [||]

### MachineState
#### Types
* `type MachineState = { Registers: Registers ; StatusBits: Flags ; Memory  }` record of registers, status bits and memory ( unimplemented ).

#### Functions
* `MachineState.Registers_ : ( MachineState -> Registers ) * ( Regisers -> MachineState -> MachineState ) ` Lens for `Registers`, useful for UI.
* `MachineState.Flags_ : ( MachineState -> Flags ) * ( Flags -> MachineState -> MachineState ) ` Lens for `StatusBits`, useful for UI.
* `MachineState.Register_ : ( RegisterID -> MachineState -> Data ) * ( RegiserID -> Data -> MachineState -> MachineState ) ` Lens for `Register`, useful for Instructions implementation.
* `MachineState.Flag_ : ( FlagID -> MachineState -> Data ) * ( FlagID -> Data -> MachineState -> MachineState ) ` Lens for `Flag`, useful for Instructions implementation.
* `make : unit -> MachineState` MachineState initialiser - it will be called only once in every execution of the assembly code.