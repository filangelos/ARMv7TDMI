namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: MachineState
    Description: 
*)

module MachineState =

    type MachineState =
        // Registers
        { Registers: Registers
        // Status Bits or NZCV Flags
          StatusBits: Flags }

        /// Registers Optic Function
        static member Registers_ =
            ( fun state -> state.Registers ), ( fun registers state -> { state with Registers = registers } )

        /// Flags Optic Function
        static member Flags_ =
            ( fun state -> state.StatusBits ), ( fun flags state -> { state with StatusBits = flags } )

        /// Register Composition Optic Function
        static member Register_ =
            // Getter: RegisterID -> MachineState -> Data
            ( fun (id: RegisterID) (state: MachineState) -> state.Registers.Item id ),
            // Setter: RegisterID -> Data -> MachineState -> MachineState
            ( fun (id: RegisterID) (value: Data) (state: MachineState) -> { state with Registers = Map.add id value state.Registers } )

        /// Flag Composition Optic Function
        static member Flag_ =
            // Getter: RegisterID -> MachineState -> Data
            ( fun (id: FlagID) (state: MachineState) -> state.StatusBits.Item id ),
            // Setter: RegisterID -> Data -> MachineState -> MachineState
            ( fun (id: FlagID) (value: bool) (state: MachineState) -> { state with StatusBits = Map.add id value state.StatusBits } )

    /// MachineState Initialisation
    let make () : MachineState =
        let registers : Registers =
            // Enumerate all RegisterIDs
            Common.enumerator<RegisterID>
            // Initialise all Registers to zero
            |> Array.map ( fun id -> id, 0 )
            // construct Map
            |> Map.ofArray

        let flags : Flags =
            // Enumerate all Flags
            Common.enumerator<FlagID>
            // Initialise all Status Bits to zero
            |> Array.map ( fun id -> id, false )
            // construct Map
            |> Map.ofArray

        { Registers = registers ; StatusBits = flags }

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)

    // test make ()
//    let 