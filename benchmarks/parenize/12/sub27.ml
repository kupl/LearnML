type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina | Norway
type tourna = LEAF of team
	| NODE of tourna * tourna


let tagToStr tag =
	match tag with
	| Korea -> "Korea"
	| France -> "France"
	| Usa 	-> "Usa"
	| Brazil -> "Brazil"
	| Japan -> "Japen"
	| Nigeria -> "Nigeria"
	| Cameroon-> "Cameroon"
        | Poland -> "Poland"
	| Portugal -> "Portugal"
	| Italy -> "Italy"
	| Germany -> "Germany"
	| Sweden -> "Sweden"
	| England-> "England"
        | Croatia -> "Croatia"
	| Argentina-> "Argentina"
	| Norway -> "Norway"


let rec parenize tree = 
	match tree with
	| LEAF t -> (tagToStr t)
	| NODE(left, right) -> "(" ^ (parenize left) ^ " " ^ (parenize right) ^ ")"

