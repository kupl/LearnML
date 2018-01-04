(* hw1-5 *)
(* 2010-11687 Keunjun Choi *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina
type tourna = LEAF of team
            | NODE of tourna * tourna
let rec parenize tree =
	let to_string team = match team with
	| Korea -> "Korea"
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
	in
	match tree with
	| LEAF t -> to_string t
	| NODE(t1, t2) -> "("^parenize t1^" "^parenize t2^")"

