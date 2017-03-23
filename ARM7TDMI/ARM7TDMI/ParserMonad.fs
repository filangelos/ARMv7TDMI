namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser Monads

    Description: Self-implementation of Monads required to build Parser Combinators

    Guidance Sources: fsharpforfunandprofit.com, quanttec.com/fparsec/, vimeo.com/113707214, stackoverflow.com
     
    Notes: 
    1) Initially tried to use FParsec Library, not compatible with Fable,so self-implemented suite of required 
       parsing tools and combinators.

    2) Self-implemented (through research and guidance from above Sources), as parser structure not Fable
       compatible (due to lambda in Parser<'T>), but maintained for Command Line testing as too far progressed.
       
    3) Infix operators match those used in FParsec for implemented combinator functions thus making future 
       reference easy.
*)

module ParserMonad =

    open System
    open Common

////////////////////////////// Parser, Monadic Combinator implementation and Error Handling  /////////////////////////////
    
    // Position in the Array of Token Lists - InitState.lineList
    type Pos = {
        lineNo : int
        tokenNo : int
    }

    //The state of the Parser at Input
    type InitState = {
        lineList : List<Token>[]
        position : Pos
    }

    // Initialise Position
    let initPos = {lineNo=0; tokenNo=0;}

    /// Increment the tokenNo number
    let incrTok pos =  {pos with tokenNo=pos.tokenNo + 1}

    /// Increment the lineNo number and set tokenNo to 0
    let incrLine pos = {lineNo=pos.lineNo + 1; tokenNo=0}

    // Create a new InitState from a Token List
    let tokenToInit tokenLst = 
        //Use splitBy to split by TokNewLine Token
        // and return the an array of token lists
        let y = splitBy TokNewLine tokenLst true
        match y with
            | [] -> {lineList = [||]; position = initPos;}
            | _ ->  {lineList = List.toArray y; position=initPos;}

    // Return the current line or token signifying end of file (TokEOF)
    let currLine initState = 
        let linePos = initState.position.lineNo
        if linePos < initState.lineList.Length then
            initState.lineList.[linePos]
        else
            [TokEOF]

    // Decision function for nextToken return
    // 1) If lineNo >= lastLine -> return TokEOF
    // 2) If tokenNo < line length -> return token at tokPos and tokPos++
    // 3) If tokenNo = line length -> return TokNewLine, linePos++
    let decisionFunc (linePos, tokPos, input)= 
        if linePos >= input.lineList.Length then
            input, None
        else
            let currLine = currLine input
            if tokPos < currLine.Length then
                let token = currLine.[tokPos]
                let newPos = incrTok input.position 
                let newState = {input with position=newPos}
                newState, Some token
            else 
                let token = TokNewLine
                let newPos = incrLine input.position 
                let newState = {input with position=newPos}
                newState, Some token

    // Get the next token from the Initlist or return 
    // None if there are any else return End Options.
    // Then return the updated InitState
    let nextToken input =
        let linePos = input.position.lineNo
        let tokPos = input.position.tokenNo
        decisionFunc (linePos, tokPos, input)

    // String signifying that specific parser's components    
    type PLabel = string
    // String specifying the Error
    type PError = string

    // Position in the file being Parsed at any one time
    type PPosition = {
        currLine : List<Token>
        lineNo : int
        tokenNo : int
    }

    // Outcome of a Parser - either success or failure (with error message)
    type Outcome<'a> =
        | Success of 'a
        | Failure of PLabel * PError * PPosition

    // Parser Type 
    type Parser<'T> = {
        //Parses InitState and returns success or failure
        parseFunc : (InitState -> Outcome<'T * InitState>)
        //Label of individual parser in pipeline
        pLabel:  PLabel
        }
        
    // Returns a PPosition for the next parser in the pipeline  
    let parserPosfromInitState initState = {
        currLine =  currLine initState
        lineNo = initState.position.lineNo
        tokenNo = initState.position.tokenNo
    }

    // Set Label for Parser
    let setLabel parser newLab = 
        // Inner function to update to change label to next in pipeline
        let innerFunc input = 
            let result = parser.parseFunc input
            match result with
            | Success s -> Success s 
            // If we fail, return label for new parser in pipeline
            | Failure (oldLab,err, parPos) -> Failure (newLab,err, parPos)
        // Return new Parser
        {parseFunc=innerFunc; pLabel=newLab}

    // Infix operator for setting label
    let ( <?> ) = setLabel

    // Outer function that gets predicate to be matched and its label
    let satisfy pred lab =
        // inner function consumes next token in token list
        let innerFunc tokenLst=
            let remainInput, tokenOpt = nextToken tokenLst
            match tokenOpt with 
                //No token in list - return Failure
                | None -> let err = "No more input"
                          let pos = parserPosfromInitState tokenLst
                          printf "%A" (nextToken tokenLst)                                                              
                          Failure (lab,err,pos) 
                //Check if token matches predicate
                | Some first -> 
                    if pred first then
                        Success (first,remainInput)
                    else
                        let err = sprintf "Unexpected '%A'" first
                        let pos = parserPosfromInitState tokenLst
                        Failure (lab,err,pos)
        // return the parser
        {parseFunc=innerFunc; pLabel=lab}

    
    // Run a specific parser on a Token
    let runInput parser input = 
        parser.parseFunc input

    // Run successice parsers on a Token List
    let run parser inputTokenLst = 
        runInput parser (tokenToInit inputTokenLst)

    // Parse a specific Token defined by tokentoMatch
    let pToken tokenToMatch = 
        let pred tk = (tk = tokenToMatch) 
        let label = sprintf "%A" tokenToMatch 
        satisfy pred label 
 
    /// This is akin to a standard compose function >>, but takes a 
    /// function which produces a parser (func) , and a parser
    /// and then passes the output of parser into func, to create a new parser
    let bindParse func parser =
        let label = "Nothing"
        let innerFn input =
            let res1 = runInput parser input 
            match res1 with
            | Failure (label, err, pos) -> 
                // return error from parser1
                Failure (label, err, pos) 
            | Success (val1,remInput) ->
                // apply the function f to get a new a parser value
                let p2 = func val1
                // run parser with remaining input
                runInput p2 remInput
        {parseFunc =innerFn; pLabel=label}

    //Infix operator for binding
    let ( >>= ) parser func = bindParse func parser

    // Takes in a value and returns a parser of that value 
    let returnParse x = 
        let innerFunc input =
            // Ignore the input 
            Success (x,input)
        // return the inner function
        {parseFunc =innerFunc; pLabel= "Success"} 

    /// apply a function to the value inside a parser
    let mapParse func = bindParse (func >> returnParse)

    /// Parser A followed by Parser B
    let (.>>.) parseA parseB =   
        let label = sprintf "%A followedBy %A" (parseA.pLabel) (parseB.pLabel)      
        parseA >>= (fun parseAResult -> 
            parseB >>= (fun parseBResult -> returnParse (parseAResult,parseBResult) )) <?> label

    /// Parser A or Parser B
    let ( <|> ) parseA parseB =
        let innerFunc input =
            // Run Parser A
            let resultA = runInput parseA input
            // Inspect Outcome of Parse A
            match resultA with
                // Return Parser A if successful
                | Success result -> resultA
                // Try Parser B if failure
                | Failure (label, err, pos) -> 
                    let resultB = runInput parseB input
                    resultB
        // Return either Parser A or Result of B
        {parseFunc =innerFunc; pLabel = parseA.pLabel;}

    /// Choose from a list of Parsers
    let choice parserList = 
        List.reduce ( <|> ) parserList

    /// Choose from a list of Tokens
    let anyOf tokenList = 
        let label = sprintf "anyOf %A" tokenList
        tokenList
        |> List.map pToken // convert into parsers
        |> choice
        <?> label

    /// Optional instance of parser P
    let opt p = 
        let exists = mapParse Some p
        let none = returnParse None
        // Use choice to see if it exists
        exists <|> none

    /// Return only the result of first parser in pair
    let (.>>) parseA parseB = 
        // Parse A then B
        parseA .>>. parseB
        // Keep first value
        |> mapParse (fun (a,b) -> a) 

    /// Return only the result of second Parser in pair
    let (>>.) parseA parseB = 
        // Parse A then B
        parseA .>>. parseB
        // Keep second value
        |> mapParse (fun (a,b) -> b) 

    /// Return result of parser in middle
    let inBetween pA pB pC = 
        pA >>. pB .>> pC
    
    // Ignore output of Parser only check if satisfied
    let (>>%) parse t = mapParse (fun _ -> t) parse

    /// Matches 0+ instances of Parser<'T>
    let zeroPlus parser = 
        let label = sprintf "0+ %s" (parser.pLabel)
        // Recursive Parser keeps running until no more of 'T or failure
        let rec pZeroPlus parser input =
            let initResult = runInput parser input 
            // Match outcome with Failure or Success
            match initResult with
            // Parse failure - return empty list
            | Failure (_) ->  ([],input)  
            // Parse success - recursive call to get subsequent outcome (value)
            | Success (initValue, firstParseOutcome) -> 
                let (nextValues,remainingInput) = 
                    pZeroPlus parser firstParseOutcome
                let values = initValue::nextValues
                (values,remainingInput) 
        // Recursive inner function to output pZeroplus parse
        let rec innerFunc input =
            // Parse the input -- always succeeds, so can be wrapped in Success
            Success (pZeroPlus parser input)
        {parseFunc=innerFunc; pLabel=label}

    /// Matches 1+ instances of the specified Parser<'T> - built using zeroPlus parser
    let onePlus p =         
        let label = sprintf "1+ %s" (p.pLabel)
        p >>= (fun h -> zeroPlus p >>= (fun tl -> returnParse(h::tl))) <?> label