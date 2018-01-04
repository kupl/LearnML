type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
            | Poland | Portugal | Italy | Germany | Sweden | England
            | Croatia | Argentina
type tourna = LEAF of team
              | NODE of tourna * tourna


let rec parenize var_tourna =
  match var_tourna with
    LEAF var_team -> 
    (
      match var_team with
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
        |Argentina -> "Argentina"
    )
    |NODE (var1, var2) -> (String.concat "" ("("::(parenize var1)::" "::(parenize var2)::[")"]))
