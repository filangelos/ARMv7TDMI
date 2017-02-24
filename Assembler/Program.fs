// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Assembler

module Program =
    [<EntryPoint>]
    let main argv = 
        printfn "Running tokeniseTest:"
        Tokeniser.tokeniseTest
        printfn "Finished tokeniseTest"
        0 // return an integer exit code