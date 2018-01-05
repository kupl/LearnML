type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
                | NODE of tourna * tourna


let drop ((old : tourna), (loser : team)) =

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
       |NODE(a,b) -> "(" ^ toParen(a) ^ " " ^ toParen(b) ^ ")" in

 let rec drop_tourna ((old : tourna), (loser : team)) =
      match old with
         LEAF a ->LEAF a
       | NODE(a,b) -> if a=(LEAF loser) then drop_tourna(b, loser)
                              else if b=(LEAF loser) then drop_tourna(a, loser)
                              else NODE( (drop_tourna(a, loser)), (drop_tourna(b, loser))) in

match drop_tourna(old, loser) with
        LEAF a -> if a=loser then ""
                       else toParen(drop_tourna(old, loser))
       | _ -> toParen(drop_tourna(old, loser));;
         