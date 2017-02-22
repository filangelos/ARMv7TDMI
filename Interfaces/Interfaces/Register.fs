namespace Interfaces

module Register =

    // Register Type
    type Register = 
//      private
        // Register ID
        { id: string
        // Register Data Content
          value: Data }
      
        // ID Optic Functions 
        static member id_ =
          ( fun reg -> reg.id ), ( fun reg id -> { reg with id = id } )

        // Value Optic Functions 
        static member value_ =
          ( fun reg -> reg.value ), ( fun reg value -> { reg with value = value } )

    // Operand Type
    type Operand =
        // Data Content of Register
        | ORegister of Register
        // Literal Passed by Value
        | OLiteral of Data
