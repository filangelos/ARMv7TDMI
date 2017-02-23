namespace Interfaces

module MachineState =

    open Register
    open Flag
    open MemoryLocation

    type MachineState =
        // Registers
        { Registers: Register seq
        // Status Bits or NZCV Flags
          StatusBits: Flag list
        // Raw Memory
          Memory: MemoryLocation array }

        // Registers Optic Function
        static member Registers_ =
            ( fun state -> state.Registers ), ( fun state regs -> { state with Registers = regs } )

    // MachineState Initialisation
    let make () =
        let registers =
            [ 0 .. 15 ]
            // Register ID string generator
            |> List.map ( fun i -> ( "R" + (string i) ) )
            // Register ID constructor
            |> List.map ( fun id -> Register.make id )
        let flags =
            [ 'N'; 'Z'; 'C'; 'V' ]
            // Flag constructor
            |> List.map ( fun id -> Flag.make id )
        // Unimplemented !!!!!!!
        let memory = Array.init 4096 ( fun x -> 0u )
        { Registers = registers; StatusBits = flags; Memory = memory }