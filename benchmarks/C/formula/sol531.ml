type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec arith f =
  match f with
  |Num n -> n
  |Plus (n, m) -> (arith n) + (arith m)
  |Minus (n, m) -> (arith n) - (arith m)

let rec eval form =
  match form with
  |True -> true
  |False -> false
  |Not p -> if eval p = true
            then false
            else true
  |AndAlso (p,q) -> if eval p = true
                  then (if eval q =true
                        then true
                        else false)
                  else false
  |OrElse (p,q) -> if eval p = true
                  then true
                  else (if eval q =true
                          then true
                          else false)
  |Imply (p,q) -> if eval p = true
                  then (if eval q = true
                          then true
                          else false)
                  else true

  |Equal (n, m) -> if arith (Minus (n,m)) = 0
                  then true
                  else false


  



