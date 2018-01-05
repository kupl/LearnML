type team = Korea | France | Usa | Brazil | Japan | Nigeria
         | Cameroon | Poland | Portugal | Italy | Germany
         | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
           | NODE of tourna * tourna

let rec parenize(tourna) =
   match tourna with
   | LEAF Korea -> "Korea"
   | LEAF France -> "France"
   | LEAF Usa -> "Usa"
   | LEAF Brazil -> "Brazil"
   | LEAF Japan -> "Japan"
   | LEAF Nigeria -> "Nigeria"
   | LEAF Cameroon -> "Cameroon"
   | LEAF Poland -> "Poland"
   | LEAF Portugal -> "Portugal"
   | LEAF Italy -> "Italy"
   | LEAF Germany -> "Germany"
   | LEAF Sweden -> "Sweden"
   | LEAF England -> "England"
   | LEAF Croatia -> "Croatia"
   | LEAF Argentina -> "Argentina"
   | NODE(a, b) -> "(" ^ parenize(a) ^ " " ^ parenize(b) ^ ")"
(*
let a1 = LEAF Korea
let a2 = NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)
let test1 = parenize(a1)
let test2 = parenize(a2)

let _ = print_string test1;
        print_newline()
        print_string test2;
        print_newline()
*)
