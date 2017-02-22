namespace Interfaces

[<AutoOpen>]
module Optics =

    // Lens Type
    type Lens<'a, 'b> = ( 'a -> 'b ) * ( 'b -> 'a -> 'a )

    // Get Function
    let inline get lens = fst lens

    // Set Function
    let inline set lens = snd lens