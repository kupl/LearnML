type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec parenize tour =
	let parenizeLeaf t =
		match t with
		|Korea		-> "Korea"
		|France		-> "France"
		|Usa		-> "Usa"
		|Brazil		-> "Brazil"
		|Japan		-> "Japan"
		|Nigeria	-> "Nigeria"
		|Cameroon	-> "Cameroon"
		|Poland		-> "Poland"
		|Portugal	-> "Portugal"
		|Italy		-> "Italy"
		|Germany	-> "Germany"
		|Norway		-> "Norway"
		|Sweden		-> "Sweden"
		|England	-> "England"
		|Argentina	-> "Argentina"
	in
	match tour with
	LEAF a -> parenizeLeaf a
	|NODE (a, b) ->	"("^(parenize a)^" "^(parenize b)^")"

