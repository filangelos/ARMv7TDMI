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

    open Expecto

    /// enumerate all values of a D.U. 
    let inline enumerator<'T> =
        FSharp.Reflection.FSharpType.GetUnionCases(typeof<'T>)
        |> Array.map (fun c ->  Reflection.FSharpValue.MakeUnion(c,[||]) :?> 'T)

    /// Split a list to a list of lists at the delimiter (del)
    let inline splitBy (del: 'a) (lst: 'a list) (remove: bool) : ('a list) list =

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
              match remove with
              | true  -> yield! yieldRevNonEmpty acc
              | false -> yield! yieldRevNonEmpty (h::acc)
              yield! loop [] t
            | h::t ->
              yield! loop (h::acc) t }
          
        loop [] lst |> List.ofSeq

(*----------------------------------------------------------- Testing -----------------------------------------------------------*)

    // initialise sandbox Type Dummy
    type private Dummy = | X | Y | Z | A | B

    /// Complete Module Unit Testing
    let testToolbox : Test =

        // enumerator
        let testEnumerator : Test =
            test "Enumerator" { Expect.equal enumerator<Dummy> [|X;Y;Z;A;B|] "should be [|X;Y;Z;A;B|]" }

        // byte Optics test
        let testSplitBy : Test =
            testList "SplitBy" 
                [ test "remove <- true" 
                    { let dl : int list = [ 1 ; 13 ; 8 ; 0 ; 3 ; 4 ; 0 ; 1 ]
                      let el : int list list = [ [ 1 ; 13 ; 8 ] ; [ 3 ; 4 ] ; [ 1 ] ]
                      Expect.equal (splitBy 0 dl true) el "should be [ [ 1 ; 13 ; 8 ] ; [ 3 ; 4 ] ; [ 1 ] ]" }
                  test "remove <- false"
                    { let dl : int list = [ 1 ; 13 ; 8 ; 0 ; 3 ; 4 ; 0 ; 1 ]
                      let el : int list list = [ [ 1 ; 13 ; 8 ; 0 ] ; [ 3 ; 4 ; 0 ] ; [ 1 ] ]
                      Expect.equal (splitBy 0 dl false) el "should be [ [ 1 ; 13 ; 8 ; 0 ] ; [ 3 ; 4 ; 0 ] ; [ 1 ] ]" } ] 

        testList "Top Level" [ testSplitBy ; ]