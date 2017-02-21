namespace Interfaces

module Program =

    open FlagsModule

    [<EntryPoint>]
    let main argv =
        let f =  { C = C true; V = V true; N = N true; Z = Z true } 
        printfn "%A" f
        0 // return an integer exit code
