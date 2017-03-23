# Parser

## Description
The Parser is implemented using two files - Parser.fs and ParserMonad.fs
* Parser.fs contains the initial functionality for the 



## Modules




## Interfacing and Project Management

- We used  Facebook Messenger (calling and IMs) to communicate and manage the project
- As our modules were extremely well defined from the start we only needed to know who was behind us  and in front of us in the pipeline (Fable aside)
- In my case this was Baron – we decided that I would receive a Token List from the Tokeniser and return an Instruction List to the AST.
- Thus whenever we changed Common we would only need to communicate with each other unless there were some architectural changes (which was common at the beginning) – in this case, we would message the entire group to make sure it was safe to Push an updated common. Usually, only the stakeholders at the time would respond.


## Project Milestones

- [x] Figure out what I am supposed to do
- [x] Figure out how I am supposed to start
- [x] Attempt to build simple Match Parser
- [x] Build basic token parser
- [ ] Implement FParsec (turned out to be impossible)
- [x] Self-Implement basic Monads for Parser Combinators
- [x] Implement map function for each parser
- [x] Write ADC function
- [x] Write flag setting function
- [x] Come up with machineState interface
- [x] Write basic tokeniser
- [x] `MachineState` Testing
- [x] Basic UI
- [x] Write basic parser
- [x] Write basic AST
- [x] Connect all basic modules together
- [x] Write flag-setting function
- [x] Support for all arithmetic instructions
- [x] Error Handling Framework - "Printing" Monad
- [x] Memory Implementation
## Types

*
*
---
## Functions

*
*
---
## Dependencies