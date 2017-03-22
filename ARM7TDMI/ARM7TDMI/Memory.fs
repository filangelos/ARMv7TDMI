namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Memory
    Description: Model for Abstract Data Type `Memory`, simulating the memory of the processor, 
                 address->byte, instructions stored in memory, label map.
                 Immutable data structure -> get<|>set access using Optics.
*)

[<AutoOpen>]
module Memory =

    open Expecto

    type Memory =
        // AST
        { AST: AST
        // Raw memory storage
          Storage: Map<Address, byte>
        // Label map
          Labels: LabelMap }

        /// AST Optic Function
        static member AST_ =
            ( fun (memory: Memory) -> memory.AST ), 
            ( fun (ast: AST) (memory: Memory) -> { memory with AST = ast } )

        /// Storage Optic Function
        static member Storage_ =
            ( fun (memory: Memory) -> memory.Storage ), 
            ( fun (storage: Map<Address, byte>) (memory: Memory) -> { memory with Storage = storage } )

        /// Labels Optic Function
        static member Labels_ =
            ( fun (memory: Memory) -> memory.Labels ), 
            ( fun (labels: LabelMap) (memory: Memory) -> { memory with Labels = labels } )

        /// initialise a memory location with zero if not accessed before
        static member private getOrZero (address: Address) (storage: Map<Address, byte>) : byte =
            // check if this address is accessed before
            match Map.containsKey address storage with
            // if already in the storage return true value
            | true -> storage.Item address
            // else return 0
            | false -> 0uy

        /// Byte Access Composition Optic Function
        static member Byte_ =
            // Getter: Address -> Memory -> byte
            ( fun (address: Address) (memory: Memory) -> Memory.getOrZero address memory.Storage),
            // Setter: Address -> byte -> Memory -> Memory
            ( fun (address: Address) (value: byte) (memory: Memory) -> 
                { memory with Storage = Map.add address value memory.Storage } )

        /// Word Access Composition Optic Function
        static member Word_ =
            // Getter: Address -> Memory -> Data
            ( fun (address: Address) (memory: Memory) -> 
                match (address % 4 = 0) with
                | true ->
                    let raw =
                        // fetch memory 4-byte content and construct an int
                        [| Optics.get Memory.Byte_  address      memory
                           Optics.get Memory.Byte_ (address + 1) memory
                           Optics.get Memory.Byte_ (address + 2) memory
                           Optics.get Memory.Byte_ (address + 3) memory |]
                    int raw.[0] ||| ((int raw.[1]) <<< 8) + ((int raw.[2]) <<< 16) + ((int raw.[3]) <<< 24)
                | false -> 0 ),
            // Setter: Address -> Data -> Memory -> MachineState
            ( fun (address: Address) (value: Data) (memory: Memory) ->
                match (address % 4 = 0) with
                | true ->
                    // Maps Merger Function
                    let merge (p:Map<'a,'b>) (q:seq<'a*'b>) = 
                        Map(Seq.concat [ (Map.toSeq p) ; q ])
                    // Raw Bytes to be copied
                    let raw : (Address * byte) [] =
                        [| address,     byte  value
                           address + 1, byte (value >>> 8)
                           address + 2, byte (value >>> 16)
                           address + 3, byte (value >>> 24) |]
                    { memory with Storage = merge memory.Storage raw }
                | false -> memory )

        /// Address Composition Optic Function
        static member Label_ =
            // Getter: Label -> Memory -> Address
            ( fun (label: string) (memory: Memory) -> Map.find label (Optics.get Memory.Labels_ memory) ),
            // Setter: Label -> Address -> Memory -> Memory
            ( fun (label: string) (value: Address) (memory: Memory) -> { memory with Labels = Map.add label value memory.Labels } )

        /// Initialise Memory Content with DCD and FILL instructions
        static member push =
            // Setter: Data -> Memory -> Memory
            ( fun (content: Data) (memory: Memory) -> 
                // Get max used address
                let max : Address = memory |> Optics.get Memory.Storage_ |> Map.toList |> List.maxBy fst |> fst
                match max >= 0x1700 with
                | true -> Optics.set Memory.Word_ (max + 1) content memory
                | false -> Optics.set Memory.Word_ 0x1700 content memory )

    /// Memory Initialisation
    let make ((ast, labels): AST*LabelMap) : Memory =
        { AST = ast ; Storage = Map.empty<Address, byte> ; Labels = labels }

    let makeEmpty () : Memory =
        // init with empty data structures
        { AST = [] ; Storage = Map.empty<Address, byte> ; Labels = Map.empty<string, Address> }

    let makeByte (address: Address) (content: byte) : Memory =
        let storage = Map.ofList [address, content]
        // init with empty data structures
        { AST = [] ; Storage = storage ; Labels = Map.empty<string, Address> }

    let makeWord (address: Address) (content: Data) : Memory =
        let init = makeEmpty ()
        let tmp = Optics.set Memory.Word_ address content init
        // init with empty data structures
        { AST = [] ; Storage = tmp.Storage ; Labels = Map.empty<string, Address> }

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)

    /// Complete Module Unit Testing
    let testMemory : Test =
        // initialise sandbox-dummy Memory
        let sand : Memory = makeEmpty ()
        // initialiser && Optics test
        let testMake : Test =
            testList "Initialisers" 
                [ test "AST_" { Expect.equal (Optics.get Memory.AST_ sand) [] "empty initialisation" }
                  test "Storage_" { Expect.equal (Optics.get Memory.Storage_ sand) Map.empty<Address, byte> "empty initialisation" }
                  test "Labels_" { Expect.equal (Optics.get Memory.Labels_ sand) Map.empty<string, Address> "empty initialisation" } ] 

        // byte Optics test
        let testByte : Test =
            testList "Byte_" 
                [ test "get" { Expect.equal (Optics.get Memory.Byte_ (System.Random().Next()) sand) 0uy "should be 0uy" }
                  test "set" { 
                    let uy : Memory = makeByte 400 6uy
                    Expect.equal (Optics.set Memory.Byte_ 400 6uy sand) uy "should be 6uy" } ] 

        // word Optics test
        let testWord : Test =
            testList "Word_" 
                [ test "get" { Expect.equal (Optics.get Memory.Word_ (4*System.Random().Next()) sand) 0 "should be 0" }
                  test "set" 
                    { let dt : Memory = makeWord 400 256
                      Expect.equal (Optics.set Memory.Word_ 400 256 sand) dt "should be 256" } ] 

        // initialiser - top level Optics test
        let testLabel : Test =
            testList "Label_" 
                [ test "get" { Expect.equal (0) -1 "unimplemented" }
                  test "set" { Expect.equal (0) -1 "unimplemented" } ] 

        testList "Top Level" [ testMake ; testByte ; testWord ; testLabel ]