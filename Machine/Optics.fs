namespace Machine

(* 
	High Level Programming @ Imperial College London # Spring 2017
	Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

	Contributors: Angelos Filos

	Module: Optics
	Description: 
*)

[<AutoOpen>]
module Optics =

    // Lens Type
    type Lens<'a, 'b> = ( 'a -> 'b ) * ( 'b -> 'a -> 'a )

    // Get Function
    let inline get lens = fst lens

    // Set Function
    let inline set lens = snd lens
