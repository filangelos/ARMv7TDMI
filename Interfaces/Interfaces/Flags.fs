namespace Interfaces

module FlagsModule =

    type Flag =
        | C of bool
        | V of bool
        | N of bool
        | Z of bool

    // Machine State Flags
    type Flags = { C: Flag; V: Flag; N: Flag; Z: Flag }

//    module Flags =

        // Get Flag
//        let getFlag ( f: Flag ) ( flags: Flags ) =
//            match f with
//            | C v ->  flags.C
            

        // Make new Flags with CVNZ
//        let Flags ( C: bool) (V: bool ) ( N: bool ) ( Z: bool ) =
//            { C = C; V = V; N = N; Z = Z }

// Exported Type
//type Flags = FlagsModule.Flags