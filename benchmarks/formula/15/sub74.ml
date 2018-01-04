type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let eval : formula -> bool = fun a ->
   match a with 
   |TRUE -> true
   |FALSE -> false
   |(NOT TRUE) -> false
   |(NOT FALSE) -> true
   |ANDALSO(TRUE, TRUE) -> true
   |ANDALSO(TRUE,FALSE) -> false
   |ANDALSO(FALSE, TRUE) -> false
   |ANDALSO(FALSE, FALSE) -> false
   |ORELSE(TRUE, TRUE) -> true
   |ORELSE(TRUE, FALSE) -> true
   |ORELSE(FALSE, TRUE) -> true
   |ORELSE(FALSE, FALSE) -> false
   |IMPLY(TRUE, FALSE) -> false
   |IMPLY(TRUE, TRUE) -> true
   |IMPLY(FALSE, TRUE) -> true
   |IMPLY(FALSE, FALSE) -> true
   |LESS(NUM b,NUM c) ->
     if b > c then false
     else true
   |LESS(PLUS(NUM b, NUM c), PLUS(NUM d, NUM e)) ->
     let x = b + c
     and y = d + e
     in
       if x > y then false
       else true
   |LESS(PLUS(NUM b, NUM c), MINUS(NUM d, NUM e)) ->
     let x = b + c
     and y = d - e
     in
       if x > y then false
       else true
   |LESS(MINUS(NUM b, NUM c), PLUS(NUM d, NUM e)) ->
     let x = b - c
     and y = d + e
     in
       if x > y then false
       else true
   |LESS(MINUS(NUM b, NUM c), MINUS(NUM d, NUM e)) ->
     let x = b - c
     and y = d - e
     in
       if x > y then false
       else true

