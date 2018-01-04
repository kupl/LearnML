(*Parenize*)
(*http://ropas.snu.ac.kr/~kwang/4190.310/09/hw2.pdf 1번*)
type team = Korea	|France	|Usa	|Brazil	|Japan	|Nigeria	|Cameroon	|Poland	|Portugal
	|Italy	|Germany	|Sweden	|England	|Croatia	|Argentina

type tourna = 
	LEAF of team
	|NODE of tourna * tourna

let team2str : team -> string
= fun team ->
	match team with
	Korea -> "Korea"	
	|France	-> "France"
	|Usa -> "Usa"
	|Brazil	-> "Brazil"
	|Japan -> "Japan"
	|Nigeria -> "Nigeria"
	|Cameroon	-> "Cameroon"
	|Poland	-> "Poland"
	|Portugal -> "Portugal"
	|Italy -> "Italy"
	|Germany -> "Germany"
	|Sweden -> "Sweden"
	|England -> "England"
	|Croatia -> "Croatia"
	|Argentina -> "Argentina"

let rec toParen: tourna -> string
= fun tour ->
	match tour with
	|LEAF team -> team2str team
	|NODE (t1,t2) -> "("^(toParen t1)^" "^(toParen t2)^")"


	