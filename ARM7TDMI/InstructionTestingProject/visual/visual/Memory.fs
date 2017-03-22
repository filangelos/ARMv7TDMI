namespace VisualInterface

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: MachineState
    Description: 
*)

[<AutoOpen>]
module Memory =

    type Memory =
        // AST
        { AST: AST
        // Raw memory storage
          Storage: Map<Address, byte> }

        /// AST Optic Function
        static member AST_ =
            ( fun (memory: Memory) -> memory.AST ), 
            ( fun (ast:AST) (memory: Memory) -> { memory with AST = ast } )

        /// Storage Optic Function
        static member Storage_ =
            ( fun (memory: Memory) -> memory.Storage ), 
            ( fun (storage: Map<Address, byte>) (memory: Memory) -> { memory with Storage = storage } )

        /// initialise a memory location with zero if not accessed before
        static member private cleanGet (address: Address) (storage: Map<Address, byte>) : byte =
            match Map.containsKey address storage with
            // if already in the storage return true value
            | true -> storage.Item address
            // else return 0
            | false -> 0uy

        /// Byte Access Composition Optic Function
        static member Byte_ =
            // Getter: Address -> Memory -> byte
            ( fun (address: Address) (memory: Memory) -> Memory.cleanGet address memory.Storage),
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
                        [| Optics.get Memory.Byte_ address memory
                           Optics.get Memory.Byte_ (address + 1) memory
                           Optics.get Memory.Byte_ (address + 2) memory
                           Optics.get Memory.Byte_ (address + 3) memory |]
                    int raw.[0] ||| ((int raw.[1]) <<< 8) + ((int raw.[2]) <<< 16) + ((int raw.[3]) <<< 24)
                | false -> failwith "incorrect address passed" ),
            // Setter: Address -> Data -> Memory -> MachineState
            ( fun (address: Address) (value: Data) (memory: Memory) ->
                match (address % 4 = 0) with
                | true ->
                    // Maps Merger Function
                    let merge (p:Map<'a,'b>) (q:seq<'a*'b>) = 
                        Map(Seq.concat [ (Map.toSeq p) ; q ])
                    // Raw Bytes to be copied
                    let raw : (Address * byte) [] =
                        [| address, byte value
                           address + 1, byte (value >>> 8)
                           address + 2, byte (value >>> 16)
                           address + 3, byte (value >>> 24) |]
                    { memory with Storage = merge memory.Storage raw }
                | false -> failwith "incorrect address passed" )

    /// Memory Initialisation
    let make (ast: AST) : Memory =
        { AST = ast ; Storage = Map.empty<Address, byte> }

    let makeHack () : Memory =
        { AST = ([], Map.empty<string, Address>) ; Storage = Map.empty<Address, byte> }

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)