# MachineState

## Types
* `type MachineState = { Registers: Registers ; StatusBits: Flags ; Memory  }` record of registers, status bits and memory ( unimplemented ).

## Functions
* `MachineState.Registers_ : ( MachineState -> Registers ) * ( Regisers -> MachineState -> MachineState ) ` Lens for `Registers`, useful for UI.
* `MachineState.Flags_ : ( MachineState -> Flags ) * ( Flags -> MachineState -> MachineState ) ` Lens for `StatusBits`, useful for UI.
* `MachineState.Register_ : ( RegisterID -> MachineState -> Data ) * ( RegiserID -> Data -> MachineState -> MachineState ) ` Lens for `Register`, useful for Instructions implementation.
* `MachineState.Flag_ : ( FlagID -> MachineState -> Data ) * ( FlagID -> Data -> MachineState -> MachineState ) ` Lens for `Flag`, useful for Instructions implementation.
* `make : unit -> MachineState` MachineState initialiser - it will be called only once in every execution of the assembly code.