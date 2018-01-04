(*hw1-4 ��ǻ�� ���к� 2008-11641 �����*) 

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
			| Poland | Portugal | Italy | Germany | Sweden | England
			| Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna
let rec parenize tr =
	let to_string a =
		match a with
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
	match tr with
	(NODE (a,b)) ->
		(String.concat "" ["(";(parenize a);" ";(parenize b);")"] )
	|(LEAF a) ->
		(to_string a)
	
