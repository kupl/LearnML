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
		| LEAF(x) -> prt x

let drop (tournain,teamin) =
	let clear = ref false in
	let rec droprec tourrec =
		match tourrec with
			NODE(x,y) ->
				(match x with
					NODE(a,b) -> 
						(match y with
							NODE(c,d) -> NODE(droprec(x),droprec(y))
							| LEAF(leaf) -> if leaf = teamin then droprec(x) else NODE(droprec(x),y)
						)
					| LEAF(leaf) -> 
						(match y with
							NODE(c,d) -> if leaf = teamin then droprec(y) else NODE(x,droprec(y))
							| LEAF(leaf2) -> if leaf = teamin then (if leaf2 = teamin then (clear:=true;NODE(x,y)) else y) else (if leaf2 = teamin then x else NODE(x,y))
						)
				)
			| _ -> tourrec
	in
	let result = droprec(tournain) in
	if !clear then "" else toParen(result);;