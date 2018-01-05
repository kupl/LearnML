open String

exception Error of string

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let rec toParen (tournain:tourna) = 
	let prt teamname =
		match teamname with
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
	in
	let strcombine(str1,str2: string * string) = 
		let strlist = ref [] in
		strlist := str2::(!strlist); strlist := str1::(!strlist); concat "" !strlist
	in
	match tournain with
		NODE(x,y) -> strcombine("(",strcombine(strcombine(toParen x, strcombine(" ",toParen y)),")"))
		| LEAF(x) -> prt x;;