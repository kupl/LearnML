type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina

type tourna = LEAF of team
| NODE of tourna * tourna

let rec drop (a,b) =

let rec toParen n=
let team2string x=
match x with
Korea -> "Korea"|
France -> "France" |
Usa -> "Usa" |
Brazil -> "Brazil" |
Japan -> "Japan" |
Nigeria -> "Nigeria" |
Cameroon -> "Cameroon" |
Poland -> "Poland" |
Portugal -> "Portugal" |
Italy -> "Italy" |
Germany -> "Germany" |
Sweden -> "Sweden" |
England -> "England" |
Croatia -> "Croatia" |
Argentina -> "Argentina"
in
match n with
LEAF t -> team2string t|
NODE(a,b) -> "("^(toParen a)^" "^(toParen b)^")"
in

let rec remove (a,b) =
match a with
NODE (l, r) ->  if l=LEAF(b) then remove(r,b)
		else if r=LEAF(b) then remove(l,b)
		else NODE ((remove(l,b)),(remove(r,b)))
| LEAF t -> LEAF t
in

match a with
LEAF t -> if t=b then "" else (toParen a)|
_ ->	if a=remove(a,b) then toParen a
	else drop(remove(a,b),b)