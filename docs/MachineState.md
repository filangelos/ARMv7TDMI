# MachineState

## Types
* `type MachineState = { Registers: Registers ; StatusBits: Flags ; Memory = Memory }` record of registers, status bits and memory.
---
## Functions
* `MachineState.Registers_ : ( MachineState -> Registers ) * ( Regisers -> MachineState -> MachineState ) ` Lens for `Registers`, useful for UI.
* `MachineState.Flags_ : ( MachineState -> Flags ) * ( Flags -> MachineState -> MachineState ) ` Lens for `StatusBits`, useful for UI.
* `MachineState.Memory_ : ( MachineState -> Memory ) * ( Memory -> MachineState -> MachineState ) ` Lens for `StatusBits`, useful for UI.
* `MachineState.AST_ : ( MachineState -> AST ) * ( AST -> MachineState -> MachineState ) ` Lens for `AST`, useful for UI.
* `MachineState.Register_ : ( RegisterID -> MachineState -> Data ) * ( RegiserID -> Data -> MachineState -> MachineState ) ` Lens for `Register`, useful for Instructions implementation.
* `MachineState.Flag_ : ( FlagID -> MachineState -> Data ) * ( FlagID -> Data -> MachineState -> MachineState ) ` Lens for `Flag`, useful for Instructions implementation.
* `MachineState.Byte_ : ( Address -> MachineState -> byte ) * ( Address -> byte -> MachineState -> MachineState ) ` 
Lens for `MemoryLocation`, `get<|>set` of memory location (byte) given an address.
* `MachineState.Word_ : ( Address -> MachineState -> Data ) * ( Address -> Data -> MachineState -> MachineState ) ` 
Lens for `Data`, `get<|>set` of memory locations (word|4-bytes) given an address.
* `init : (AST * LabelMap) -> Memory` Memory initialiser - it will be called only once in every execution of the assembly code.
* `make : unit -> MachineState` MachineState initialiser - used only for individual instructions testing without complete AST.
* `initWithFlags : string -> MachineState` MachineState initialiser - used only for automating state generation for VisUAL Interface testing.
---
## Milestones
- [x] Optics Wrappers
- [x] Flag - ALU Functions
- [x] Empty Constructor
- [x] Init Constructor, accepting a `Memory`
- [x] Expecto Testing
---
## Dependencies
* `Common.RegisterID`
* `Common.Registers`
* `Common.FlagID`
* `Common.Flags`
* `Common.Memory`
* `Common.Data`
* `Optics.get`
* `Optics.set`
---
## Contributors
Angelos Filos
