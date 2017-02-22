namespace Interfaces

module Flag =

    // Status Flags Type
    type Flag =
        // Flag ID
        { id: char
        // Flag status bit
          bit: bool }

        // Flag id Optic Functions 
        static member id_ =
          ( fun flag -> flag.id ), ( fun flag id -> { flag with id = id } )

        // Flag bit Optic Functions 
        static member bit_ =
          ( fun flag -> flag.bit ), ( fun flag bit -> { flag with bit = bit } )

    // Constructor
    let make ( id: char ) =
        { id = id; bit = false }