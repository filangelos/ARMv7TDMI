namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos, Youssef Rizk

    Module: MachineState
    Description: 
*)

module MachineState =

    type MachineState =
        // Registers
        { Registers: Registers
        // Status Bits or NZCV Flags
          StatusBits: Flags 
        // Memory
          Memory: Memory }

        /// Registers Optic Function
        static member Registers_ =
            ( fun (state: MachineState) -> state.Registers ), 
            ( fun (registers: Registers) (state: MachineState) -> { state with Registers = registers } )

        /// Flags Optic Function
        static member Flags_ =
            ( fun (state: MachineState) -> state.StatusBits ), 
            ( fun (flags: Flags) (state: MachineState) -> { state with StatusBits = flags } )

        /// Memory Optic Function
        static member Memory_ =
            ( fun (state: MachineState) -> state.Memory ), 
            ( fun (memory: Memory) (state: MachineState) -> { state with Memory = memory } )

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

        /// Set V (overflow) Flag
        static member setOverflow =
            // Setter: Data -> Data -> MachineState -> MachineState
            ( fun (a: Data) (b: Data) (state: MachineState) ->
                // local conversion to int64 to handle overflow
                match (int64 a + int64 b) with
                // within int32 limits -> V = 0
                | sum when sum <= int64 System.Int32.MaxValue && 
                           sum >= int64 System.Int32.MinValue -> 
                       Optics.set MachineState.Flag_ V false state
                // out of int32 limits -> V = 1
                | _ -> Optics.set MachineState.Flag_ V true  state )

        /// Set Z (zero) Flag
        static member setZero =
            // Setter: Data -> MachineState -> MachineState
            ( fun (a: Data) (state: MachineState) -> Optics.set MachineState.Flag_ Z (a = 0) state )

        /// Set N (negative) Flag
        static member setNegative =
            // Setter: Data -> MachineState -> MachineState
            ( fun (a: Data) (state: MachineState) -> Optics.set MachineState.Flag_ N (a < 0) state )

        /// Set C (carry) Flag - Arithmetic Operation
        static member setCarryA =
            // Setter: operation -> Data -> Data -> MachineState -> int * MachineState
            ( fun (operation: uint64 -> uint64 -> uint64) (a: Data) (b: Data) (state: MachineState) ->
                // get the 32-bit unsigned integer and cast to uint64 for overflow check
                let result = operation (uint64 (uint32 a)) (uint64 (uint32 b))
                // Data - lower 32-bits  *  Carry Flag setting
                ( int result, Optics.set MachineState.Flag_ C ((result &&& (1uL <<< 32)) > 0uL) state) )

        /// Set C (carry) Flag - Logic Operation
        static member setCarryL =
            // Setter: uint64 -> MachineState -> int * MachineState
            ( fun (a: uint64) (state: MachineState) ->
                // Data - lower 32-bits  *  Carry Flag setting
                ( int a, Optics.set MachineState.Flag_ C ((a &&& (1uL <<< 32)) > 0uL) state) )

        /// Set C (carry) Flag - Right Shift
        static member setCarryRShift =
            ( fun (a: Data) (b: Data) (state: MachineState) ->
                (a, Optics.set MachineState.Flag_ C ((uint32 (b &&& (1 <<< 31))) > 0u) state) )

        /// Memory Byte Composition Optic Function
        static member Byte_ =
            // Getter: Address -> MachineState -> byte
            ( fun (address: Address) (state: MachineState) -> 
                Optics.get Memory.Byte_ address (Optics.get MachineState.Memory_ state) ),
            // Setter: Address -> byte -> MachineState -> MachineState
            ( fun (address: Address) (value: byte) (state: MachineState) -> 
                { state with Memory = Optics.set Memory.Byte_ address value (Optics.get MachineState.Memory_ state) } )

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

        { Registers = registers ; StatusBits = flags ; Memory = Memory.makeHack () }

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)