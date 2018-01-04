type team
	= Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Norway | Sweden | England
	| Argentina

type tourna
	= LEAF of team
	| NODE of tourna * tourna

let team2str t =
	match t with
	Korea -> "Korea"
	| France -> "France"
	| Usa -> "Usa"
	| Brazil -> "Brazil"
	| Japan -> "Japan"
	| Nigeria -> "Nigeria"
	| Cameroon -> "Cameroon"
	| Poland -> "Poland"
	| Portugal -> "Portugal"
	| Italy -> "Italy"
	| Germany -> "Germany"
	| Norway -> "Norway"
	| Sweden -> "Sweden"
	| England -> "England"
	| Argentina -> "Argentina"

let rec parenize tree =
	match tree with
	LEAF leaf -> (team2str leaf)
	| NODE (tree1, tree2) -> "(" ^ (parenize tree1) ^ " " ^ (parenize tree2) ^ ")"

(*
let _ = print_string (parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)))
*)
