type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon 
| Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina

type tourna = LEAF of team
			| NODE of tourna * tourna


exception Invalid_arguement

let rec parenize tour =
match tour with
|NODE (fir, sec) -> "(" ^ parenize fir ^ " " ^  parenize sec ^ ")"
|LEAF v -> t2s v
and
t2s a=
match a with
|Korea -> "Korea"
|France -> "France"
|Usa -> "Usa"
|Brazil -> "Brazil"
|Japan -> "Japan"
|Nigeria -> "Nigeria"
|Cameroon -> "Cameroon"
|Poland -> "Poland"
|Italy -> "Italy"
|Germany -> "Germany"
|Norway -> "Norway"
|Sweden -> "Sweden"
|England -> "England"
|Argentina -> "Argentina"
|Portugal -> "Portugal"




let rec change (tour, t)=
match tour with
|NODE (fir, sec) -> if fir = LEAF t then sec
					else if sec = LEAF t then fir
					else NODE(change(fir, t) , change(sec, t))
|LEAF a -> LEAF a
(*and
leafval a=
match a with
|LEAF v -> v
|NODE (x,y) -> (x,y)
*)	

let rec bdrop (tour, t)=
if tour = change(tour, t) then tour
else bdrop( change(tour, t), t)

let drop (tour, t)=
parenize (bdrop (tour, t))

