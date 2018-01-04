type team = Korea|France|Usa|Brazil|Japan|Nigeria|Cameroon
            |Poland|Portugal|Italy|Germany|Sweden|England
            |Croatia|Argentina
type tourna = LEAF of team
             |NODE of tourna *tourna

let rec toParen e =
     match e with
    |NODE(a,b) -> "("^toParen(a)^" "^ toParen(b)^ ")" 
    | LEAF c -> if c = Korea  then "Korea"
                 else if c = France then "France"
                 else if c = Usa then "Usa"
                 else if c = Brazil then "Brazil"
                 else if c = Japan then "Japan"
                 else if c = Nigeria then "Nigeria"
                 else if c = Cameroon then "Cameroon"
                 else if c = Poland then "Poland" 
                 else if c = Portugal then "Portugal"
                 else if c = Italy then "Italy"
                 else if c = Germany then "Germany"
                 else if c = Sweden then "Sweden"
                 else if c = England then "England"
                 else if c = Croatia then "Croatia"
                 else if c = Argentina then "Argentina"
                 else " "
