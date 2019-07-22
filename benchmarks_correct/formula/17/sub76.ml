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

let rec arith f =
  match f with
  |NUM n -> n
  |PLUS (n, m) -> (arith n) + (arith m)
  |MINUS (n, m) -> (arith n) - (arith m)

let rec eval form =
  match form with
  |TRUE -> true
  |FALSE -> false
  |NOT p -> if eval p = true
            then false
            else true
  |ANDALSO (p,q) -> if eval p = true
                  then (if eval q =true
                        then true
                        else false)
                  else false
  |ORELSE (p,q) -> if eval p = true
                  then true
                  else (if eval q =true
                          then true
                          else false)
  |IMPLY (p,q) -> if eval p = true
                  then (if eval q = true
                          then true
                          else false)
                  else true

  |LESS (n, m) -> if arith (MINUS (n,m)) < 0
                  then true
                  else false


  



