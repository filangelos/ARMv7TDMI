namespace ARM7TDMI

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

        /// Register Composition Optic Function
        static member DataByte_ =
            // Getter: Address -> Memory -> byte
            ( fun (address: Address) (memory: Memory) -> memory.Storage.Item address ),
            // Setter: Address -> byte -> Memory -> MachineState
            ( fun (address: Address) (value: byte) (memory: Memory) -> 
                { memory with Storage = Map.add address value memory.Storage } )

    /// Memory Initialisation
    let make (ast: AST) : Memory =
        { AST = ast ; Storage = Map.empty<Address, byte> }

    let makeHack () : Memory =
        { AST = ([], Map.empty<string, Address>) ; Storage = Map.empty<Address, byte> }

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)