(* 2009-11824 Jieun-Jeong HW1-4 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize tn =
	let team_to_string tm =
		match tm with
		Korea		-> "Korea"
		| France 	-> "France"
		| Usa		-> "usa"
		| Brazil 	-> "Brazil"
		| Japan 	-> "Japan"
		| Nigeria 	-> "Nigeria"
		| Cameroon	-> "Cameroon"
		| Poland 	-> "Poland"
		| Portugal 	-> "Portugal"
		| Italy 	-> "Italy"
		| Germany 	-> "Germany"
		| Sweden 	-> "Sweden"
		| England	-> "England"
		| Croatia 	-> "Croatia"
		| Argentina	-> "Argentina"
		| _		-> raise (Invalid_argument "@ team_to_string")
	in
	match tn with
	LEAF tm -> (team_to_string tm)
	|NODE (tnl, tnr) -> "(" ^ (parenize tnl) ^ " " ^ (parenize tnr) ^ ")"
