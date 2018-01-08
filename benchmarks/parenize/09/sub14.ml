type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina

type tourna = LEAF of team
| NODE of tourna * tourna

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