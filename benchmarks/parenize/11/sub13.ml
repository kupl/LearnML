(* PL HW1-4 "대진표 스트링"
   2007-11738
   알렉산더 *)


type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
         | Poland | Portugal | Italy | Germany | Sweden | England
         | Croatia | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

(* parenize: tourna -> string *)
let rec parenize t =
    match t with
        LEAF team -> (match team with
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
                       |Argentina -> "Argentina")

       |NODE (t1, t2) -> "(" ^ (parenize t1) ^ " " ^ (parenize t2) ^ ")"
