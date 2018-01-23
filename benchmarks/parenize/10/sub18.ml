
exception Error of string
(*Problem 4.*)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
|Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna
let maketeamstr : team -> string =
	(function thetea ->
		(match thetea with
			Korea -> "Korea"
			|France -> "France"
			|Usa -> "Usa"
			|Brazil -> "Brazil"
			|Japan -> "Japan"
			|Nigeria -> "Nigeria"
			|Cameroon -> "Cameroon"
			|Poland -> "Poland"
			|Portugal -> "Portugal"
			|Italy -> "Italy"
			|Germany -> "Germany"
			|Sweden -> "Sweden"
			|England -> "England"
			|Croatia -> "Croatia"
			|Argentina -> "Argentina"))

let rec parenize_temp : tourna -> string =
	(function thetour ->
		(match thetour with 
			LEAF le -> (maketeamstr le)
			| NODE (tou1, tou2) -> "("^(parenize_temp tou1)^" "^(parenize_temp tou2)^")"))
let parenize : tourna -> string =
	function a -> parenize_temp a