namespace Interfaces

module RegisterModule =

    open Common

    // Register Type
    type Register = private { name: string; value: Data }

    module Register =

        // Get the data content of Register
        let registerValueOf ( register: Register ) =
            register.value

        // Make new Register with name and value
        let makeRgisterWith ( name: string ) ( value: Data ) = 
            { name = name; value = value }

// Exported Type
type Register = RegisterModule.Register