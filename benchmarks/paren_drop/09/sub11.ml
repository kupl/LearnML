exception Error of string

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let rec drop((tour:tourna), (te:team)) =	
	let rec tedrop((tr:tourna), (tm:team)) =
		match tr with
		|LEAF(x) -> LEAF(x)
		|NODE(LEAF(tm), (y:tourna)) -> y
		|NODE((y:tourna), LEAF(tm)) -> y
		|NODE((y:tourna), (z:tourna)) -> NODE(tedrop(y,te),tedrop(z,te))
		
	in
	let rec toParen(x:tourna) =
		let teamtost(t:team) =
			match t with
			|Korea ->"Korea"
			|France ->"France"
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
			|Argentina -> "Argentina"
		in
		match x with
		|LEAF st -> teamtost(st)
		|NODE (a, b)-> "("^toParen(a)^" "^toParen(b)^")"
	in
	toParen(tedrop(tour, te))

