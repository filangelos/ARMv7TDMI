# Tokeniser

## Types
*
---
## Functions

* function tokenise: string -> Token list
	* Note: returns an exception from syntax error - catch this somewhere else for the web GUI (e.g. in the main)
* function (|MatchToken|_|): string -> string -> (string * string) option
* function tokeniseTest: unit
---
## Dependencies

aCommon.fs

## Notes

* If you get an exception when running tokeniseTest (after it says, "Running FSCheck..."), delete FSharp.Core from the Solution references in the Solution Explorer (Visual Studio).