type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
                | NODE of tourna * tourna

let rec toParen(tou : tourna) =
    let toString(tn : team) =
       match tn with
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
        | Argentina -> "Argentina" in

    match tou with
       LEAF(a) -> toString(a)
      |NODE(a,b) -> "(" ^ toParen(a) ^ " " ^ toParen(b) ^ ")";;
