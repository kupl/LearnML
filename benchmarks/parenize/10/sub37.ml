type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna
let rec parenize tour =
	let strinize t =
		match t with
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
		|Argentina -> "Argentina"
	in
	match tour with
	|LEAF a -> (strinize a)
	|NODE (a, b) -> String.concat ")" [(String.concat "(" ["";(String.concat " " [(parenize a); (parenize b)])] );""]
