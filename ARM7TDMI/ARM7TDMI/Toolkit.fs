namespace ARM7TDMI 

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Toolkit
    Description: 
*)

[<AutoOpen>]
module Toolkit =

    /// enumerate all values of a D.U. 
    let enumerator<'T> =
        FSharp.Reflection.FSharpType.GetUnionCases(typeof<'T>)
        |> Array.map (fun c ->  Reflection.FSharpValue.MakeUnion(c,[||]) :?> 'T)

    /// Split a list to a list of lists at the delimiter (del)
    let inline splitBy (del: 'a) (lst: 'a list) : ('a list) list =

        // reverse non-empty list
        let yieldRevNonEmpty lst = 
            match List.isEmpty lst with
            | true -> []
            | false -> [List.rev lst]
        
        // tail recursive accummulation of list using computational expressions
        let rec loop acc lst = seq {
            match lst with 
            | [] -> yield! yieldRevNonEmpty acc
            | h::t when h = del ->
              yield! yieldRevNonEmpty acc
              yield! loop [] t
            | h::t ->
              yield! loop (h::acc) t }
          
        loop [] lst |> List.ofSeq