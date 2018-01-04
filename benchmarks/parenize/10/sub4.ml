type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany 
| Sweden | England | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let a1 = LEAF(Korea) 
let a2 = LEAF(France) 
let a3 = LEAF(Usa) 
let a4 = LEAF(Brazil) 
let a5 = LEAF(Japan) 
let a6 = LEAF(Nigeria) 
let a7 = LEAF(Cameroon) 
let a8 = LEAF(Poland) 
let a9 = LEAF(Portugal) 
let a10 = LEAF(Italy) 
let a11 = LEAF(Germany) 
let a12 = LEAF(Sweden) 
let a13 = LEAF(England) 
let a14 = LEAF(Croatia) 
let a15 = LEAF(Argentina) 
let b1 = NODE(a1, a9) 
let b2 = NODE(a2, a7) 
let b3 = NODE(a4, a3) 
let b4 = NODE(a5, a15) 
let b5 = NODE(a13, a14) 
let b6 = NODE(a12, a6) 
let c1 = NODE(b1, a4) 
let c2 = NODE(b3, a2) 
let c3 = NODE(b4, a7) 
let c4 = NODE(a4, b2) 
let c5 = NODE(a14, b6) 
let c6 = NODE(a12, b4) 
let c7 = NODE(a11, b5) 
let d1 = NODE(b3, b4) 
let d2 = NODE(a11, c1) 
let d3 = NODE(c5, a3) 
let e1 = NODE(a10, d2) 
let e2 = NODE(b2, c3) 
let z1 = NODE(e1, e2) 

let rec parenize t = 
  match t with
    LEAF(Korea) -> "Korea"
    | LEAF(France) -> "(France" | LEAF(Usa) -> "Usa" | LEAF(Brazil) -> "Brazil"
    | LEAF(Japan) -> "Japan" | LEAF(Nigeria) -> "Nigeria" | LEAF(Cameroon) -> "Cameroon"
    | LEAF(Poland) -> "Poland" | LEAF(Portugal) -> "Portugal" | LEAF(Italy) -> "Italy"
    | LEAF(Germany) -> "Germany" | LEAF(Sweden) -> "Sweden" | LEAF(England) -> "England"
    | LEAF(Croatia) -> "Croatia" | LEAF(Argentina) -> "Argentina"
    | NODE (left, right) -> "(" ^ (parenize left) ^ " " ^ (parenize right) ^ ")" ;;

parenize a1;;
parenize a2;;
a1;;
parenize z1;;
