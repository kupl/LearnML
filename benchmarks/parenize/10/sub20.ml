exception Error of string
type team
  = Korea
  | France
  | Usa
  | Brazil
  | Japan
  | Nigeria
  | Cameroon
  | Poland
  | Portugal
  | Italy
  | Germany
  | Sweden
  | England
  | Croatia
  | Argentina
type tourna = LEAF of team
  | NODE of tourna * tourna

let rec parenize : tourna -> string =
  fun t ->
    try (
      match t
      with LEAF(t) -> 
        (
          match t
          with Korea -> "Korea"
            | France -> "France"
            | Usa-> "Usa"
            | Brazil-> "Brazil"
            | Japan-> "Japan"
            | Nigeria-> "Nigeria"
            | Cameroon-> "Cameroon"
            | Poland-> "Poland"
            | Portugal-> "Portugal"
            | Italy-> "Italy"
            | Germany-> "Germany"
            | Sweden-> "Sweden"
            | England-> "England"
            | Croatia-> "Croatia"
            | Argentina-> "Argentina"
        )
        | NODE(t1,t2) -> "("^(parenize t1)^" "^(parenize t2)^")"
    )
    with Error s -> raise (Error s)
      | _ -> raise (Error "cake is a lie")
