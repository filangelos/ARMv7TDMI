﻿namespace Machine

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

        // Registers Optic Function
        static member Registers_ =
            ( fun state -> state.Registers ), ( fun registers state -> { state with Registers = registers } )

        // Flags Optic Function
        static member Flags_ =
            ( fun state -> state.StatusBits ), ( fun flags state -> { state with StatusBits = flags } )

        // Register Composition Optic Function
        static member Register_ =
            // Getter: RegisterID -> MachineState -> Data
            ( fun (id: RegisterID) (state: MachineState) -> state.Registers.Item id ),
            // Setter: RegisterID -> Data -> MachineState -> MachineState
            ( fun (id: RegisterID) (value: Data) (state: MachineState) -> { state with Registers = Map.add id value state.Registers } )

        // Flag Composition Optic Function
        static member Flag_ =
            // Getter: RegisterID -> MachineState -> Data
            ( fun (id: FlagID) (state: MachineState) -> state.StatusBits.Item id ),
            // Setter: RegisterID -> Data -> MachineState -> MachineState
            ( fun (id: FlagID) (value: bool) (state: MachineState) -> { state with StatusBits = Map.add id value state.StatusBits } )

    // MachineState Initialisation
    let make () =
        let registers =
            // Initialise all Registers to zero
            Map.ofList [ R0, 0 ; R1, 0 ; R2, 0 ; R3, 0 ; R4, 0;
                         R5, 0 ; R6, 0 ; R7, 0 ; R8, 0 ; R9, 0 ;
                         R10, 0 ; R11, 0 ; R12, 0 ; R13, 0 ; R14, 0;]

        let flags =
            // Initialise all Status Bits to zero
            Map.ofList [ N, false ; Z, false ;
                         C, false ; V, false ]

        { Registers = registers ; StatusBits = flags }