namespace Interfaces

module Flags =

    // Status Flags Type
    type Flags =
        // Negative Flag
        { N: bool
        // Zero Flag
          Z: bool
        // Carry Flag
          C: bool
        // Signed Overflow Flag
          V: bool }

        // N Flag Optic Functions 
        static member N_ =
          ( fun flags -> flags.C ), ( fun flags N -> { flags with N = N } )

        // Z Flag Optic Functions 
        static member Z_ =
          ( fun flags -> flags.Z ), ( fun flags Z -> { flags with Z = Z } )

        // C Flag Optic Functions 
        static member C_ =
          ( fun flags -> flags.C ), ( fun flags C -> { flags with C = C } )

        // V Flag Optic Functions 
        static member V_ =
          ( fun flags -> flags.V ), ( fun flags V -> { flags with V = V } )