(* 200511843 LEE JONGHO *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Sweden | England
          | Croatia | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let rec string_of_team x =
	match x with
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
	| Sweden -> "Sweden"
	| England -> "England"
	| Croatia -> "Croatia"
	| Argentina -> "Argentina"

let rec parenize t =
	match t with
	LEAF a -> (string_of_team a)
	| NODE (l, r) -> "(" ^ (parenize l) ^ " " ^ (parenize r) ^ ")"

let rec pptree t =
	match t with
	LEAF a -> print_endline("|");
	| NODE (l, r) -> 