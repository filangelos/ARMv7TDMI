# Parser

## Description

The Parser is implemented using two files - Parser.fs and ParserMonad.fs
* ParserMonad.fs contains the self-implemented functionality for the Monads that make the pipelining so elegant and possible, this forms the core operations for the Parser Combinator. The vast majority of these functions can be used to create any Parser not one just for the ARM Grammar. 
    * The rest of the Parser structure and design is contained in this file. Including the parser Type information and the Railway Oriented Error Handling

* The actual Parser.fs contains the built up Parser Combinators using the ParserMonad functions. Once you understand how to build a few it becomes extremely easy to build highly complex function sets. The file is separated into: 
    * Token lists that are used by the `anyOf` or `pToken` function to implement a parser for a large D.U. types. Some of these simply contain D.U. Types and are mapped to their token in a subsequent function.
    * The main block of Parser Combinators that combine together primitive types using the infix operators and functions defined in ParserMonad.fs. They build up complex Parser Types as you progress further down. I separated the types of the Parsers as can be seen however there is significant overlap. 
    * The last part runs the parser on an input Token List using the splitBy function to parse each line independently for more powerful error handling.
    * This is followed by a testing module. This module contains a wide variety of testing that manually checks specific token lists and also uses FSCheck to automate testing, especially in the case of exceptions.

## Interfacing and Project Management

- We used  Facebook Messenger (calling and IMs) to communicate and manage the project
- As our modules were extremely well defined from the start we only needed to know who was behind us  and in front of us in the pipeline (Fable aside)
- In my case this was Baron – we decided that I would receive a Token List from the Tokeniser and return an Instruction List to the AST.
- Thus whenever we changed Common we would only need to communicate with each other unless there were some architectural changes (which was common at the beginning) – in this case, we would message the entire group to make sure it was safe to Push an updated common. Usually, only the stakeholders at the time would respond.
- See submitted documentation in Individual Interview for screenshots - I felt it not appropriate to post on public page.


## Project Milestones

- [x] Figure out what I am supposed to do
- [x] Figure out how I am supposed to start
- [x] Attempt to build simple match Parser
- [x] Build basic token parser
- [x] Study/Research Parser Combinators
- [ ] Implement FParsec (turned out to be impossible)
 -[x] Study/Research in specific detail Monads and their use in Parsers
- [x] Self-Implement basic Monads for Parser Combinators
- [x] Implement basic token parser using monads
- [x] Implement map function for each parser
- [x] Study Railway-Oriented error handling (ROeh) 
- [x] Implement primitive combinators 
- [x] Implement more complex combinators
- [x] Support for all core arithmetic Instructions
- [x] Add multiple consecituve same parser function 
- [x] Complete ROeh for arithmetic functions
- [x] Test Arithmetic functions completely
- [x] Add memory function parsers 
- [x] Test memory function parsers completely
- [x] Complete ROeh for memory functions
- [x] Complete Parser
- [x] Add stretch functionality 
- [x] Improve combinators 
- [x] Make sure everything properly documented
- [x] Full system command line Testing

## Types

*
*
---
## Functions

*
*
---
## Dependencies
* `Common.fs` - Almost all types are implemented for the Parser Combinator
* `Toolkit.splitBy` - Used to split up Token List 
* `FSCheck` - Automated Testing

## Contributors 
Pranav Prabhu
Baron Khan (Testing - 10%)