# Tokeniser

## Types

## Functions

* function tokenise: string -> Token list
	* Note: returns an exception from syntax error - catch this somewhere else for the web GUI (e.g. in the main)
* function (|MatchToken|_|): string -> string -> (string * string) option
* function tokeniseTest: unit
