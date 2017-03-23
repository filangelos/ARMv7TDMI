namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: MachineState
    Description: Model for Abstract Data Type `MachineState`, simulating the state of the processor, 
                 register values, memory locations, instructions, label map.
                 Immutable data structure -> get<|>set access using Optics.
*)

module MachineState =

    open Expecto

    type MachineState =
        // Registers
        { Registers: Registers
        // Status Bits or NZCV Flags
          StatusBits: Flags 
        // Memory
          Memory: Memory }

        (* Core Optics MachineState Functions *)

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
            ( fun (id: RegisterID) (state: MachineState) -> Map.find id (Optics.get MachineState.Registers_ state) ),
            // Setter: RegisterID -> Data -> MachineState -> MachineState
            ( fun (id: RegisterID) (value: Data) (state: MachineState) -> { state with Registers = Map.add id value state.Registers } )

        /// Flag Composition Optic Function
        static member Flag_ =
            // Getter: RegisterID -> MachineState -> Data
            ( fun (id: FlagID) (state: MachineState) -> Map.find id (Optics.get MachineState.Flags_ state) ),
            // Setter: RegisterID -> Data -> MachineState -> MachineState
            ( fun (id: FlagID) (value: bool) (state: MachineState) -> { state with StatusBits = Map.add id value state.StatusBits } )

        /// Memory AST Composition Optic Function
        static member AST_ =
            // Getter: MachineState -> AST
            ( fun (state: MachineState) -> 
                Optics.get Memory.AST_ (Optics.get MachineState.Memory_ state) ),
            // Setter: Address -> byte -> MachineState -> MachineState
            ( fun (ast: AST) (state: MachineState) -> 
                { state with Memory = Optics.set Memory.AST_ ast (Optics.get MachineState.Memory_ state) } )

        /// Memory Byte Composition Optic Function
        static member Byte_ =
            // Getter: Address -> MachineState -> byte
            ( fun (address: Address) (state: MachineState) -> 
                Optics.get Memory.Byte_ address (Optics.get MachineState.Memory_ state) ),
            // Setter: Address -> byte -> MachineState -> MachineState
            ( fun (address: Address) (value: byte) (state: MachineState) -> 
                { state with Memory = Optics.set Memory.Byte_ address value (Optics.get MachineState.Memory_ state) } )

        /// Memory Byte Composition Optic Function
        static member Word_ =
            // Getter: Address -> MachineState -> Data
            ( fun (address: Address) (state: MachineState) -> 
                Optics.get Memory.Word_ address (Optics.get MachineState.Memory_ state) ),
            // Setter: Address -> Data -> MachineState -> MachineState
            ( fun (address: Address) (value: Data) (state: MachineState) -> 
                { state with Memory = Optics.set Memory.Word_ address value (Optics.get MachineState.Memory_ state) } )

        /// LabelMap Composition Optic Function
        static member Label_ =
            // Getter: Label -> MachineState -> Address
            ( fun (label: string) (state: MachineState) -> 
                Optics.get Memory.Label_ label (Optics.get MachineState.Memory_ state) ),
            // Setter: label -> Address -> MachineState -> MachineState
            ( fun (label: string) (value: Address) (state: MachineState) -> 
                { state with Memory = Optics.set Memory.Label_ label value (Optics.get MachineState.Memory_ state) } )

        // Instruction-specific MachineState manipulation functions

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


    // Initialisers

    /// MachineState Initialisation
    let make () : MachineState =
        let registers : Registers =
            // Enumerate all RegisterIDs
            Toolkit.enumerator<RegisterID>
            // Initialise all Registers to zero
            |> Array.map ( fun id -> id, 0 )
            // construct Map
            |> Map.ofArray

        let flags : Flags =
            // Enumerate all Flags
            Toolkit.enumerator<FlagID>
            // Initialise all Status Bits to zero
            |> Array.map ( fun id -> id, false )
            // construct Map
            |> Map.ofArray

        { Registers = registers ; StatusBits = flags ; Memory = Memory.makeEmpty () }

    /// MachineState Initialisation
    let init (memory : Memory) : MachineState =
        let registers : Registers =
            // Enumerate all RegisterIDs
            Toolkit.enumerator<RegisterID>
            // Initialise all Registers to zero
            |> Array.map ( fun id -> id, 0 )
            // construct Map
            |> Map.ofArray

        let flags : Flags =
            // Enumerate all Flags
            Toolkit.enumerator<FlagID>
            // Initialise all Status Bits to zero
            |> Array.map ( fun id -> id, false )
            // construct Map
            |> Map.ofArray

        { Registers = registers ; StatusBits = flags ; Memory = memory }




    /// MachineState Initialisation - Necessary for smooth interface with VisUAL testing framework provided
    let initWithFlags (flags: string) : MachineState =
        let registers : Registers =
            // Enumerate all RegisterIDs
            Toolkit.enumerator<RegisterID>
            // Initialise all Registers to zero
            |> Array.map ( fun id -> id, 0 )
            // construct Map
            |> Map.ofArray

        let fstr : bool array =
            let f : char[] = [|for c in flags -> c|]
            Array.map ( fun c -> 
                match c with
                | '0' -> false
                | '1' -> true
                | _ -> failwithf "wrong flags string" ) f

        let flags : Flags =
            // Enumerate all Flags
            let cases = Toolkit.enumerator<FlagID>
            // Initialise Status Bits
            Array.zip cases fstr
            // construct Map
            |> Map.ofArray

        { Registers = registers ; StatusBits = flags ; Memory = Memory.makeEmpty () }

    /// MachineState Initialisation - Necessary for smooth interface with VisUAL testing framework provided
    let initWithRegister (id: RegisterID) (value: Data) : MachineState =
        let registers : Registers =
            // Enumerate all RegisterIDs
            Toolkit.enumerator<RegisterID>
            // Initialise all Registers to zero
            |> Array.map ( fun id -> id, 0 )
            // construct Map
            |> Map.ofArray

        let flags : Flags =
            // Enumerate all Flags
            Toolkit.enumerator<FlagID>
            // Initialise all Status Bits to zero
            |> Array.map ( fun id -> id, false )
            // construct Map
            |> Map.ofArray

        { Registers = Map.add id value registers  ; StatusBits = flags ; Memory = Memory.makeEmpty () }

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)

    /// Complete Module Unit Testing
    let testMachineState : Test =

        // initialise sandbox-dummy Memory
        let sand : MachineState = make ()

        // initialiser && Optics test
        let testInit : Test =
            testList "Initialisers" 
                [ test "Registers_"
                    { let regs = 
                        Array.zip [| R0;R1;R2;R3;R4;R5;R6;R7;R8;R9;R10;R11;R12;R13;R14;R15 |] (Array.init 16 (fun _ -> 0))
                        |> Map.ofArray
                      Expect.equal (Optics.get MachineState.Registers_ sand) regs "empty initialisation" }
                  test "Flags_"
                    { let flags = 
                        Array.zip [| N;Z;C;V |] (Array.init 4 (fun _ -> false))
                        |> Map.ofArray
                      Expect.equal (Optics.get MachineState.Flags_ sand) flags "empty initialisation" }
                  test "Memory_" { Expect.equal (Optics.get MachineState.Memory_ sand) (Memory.makeEmpty ()) "empty initialisation" } ] 

        // Register Optics test
        let testRegister : Test =
            testList "Register_" 
                [ test "get" { Expect.equal (Optics.get MachineState.Register_ R5 sand) 0 "should be 0" }
                  test "set" { 
                    let uy : MachineState = initWithRegister R3 13
                    Expect.equal (Optics.set MachineState.Register_ R3 13 sand) uy "should have pair <R3,13>" } ] 

        // Flag Optics test
        let testFlag : Test =
            testList "Flag" 
                [ test "get" { Expect.equal (Optics.get MachineState.Flag_ Z sand) false "should be false" }
                  test "set" { 
                    let uy : MachineState = initWithFlags "0100"
                    Expect.equal (Optics.set MachineState.Flag_ Z true sand) uy "should have pair <Z,true>" } ] 

        testList "Top Level" [ testInit ; testRegister ; testFlag ]