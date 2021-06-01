type exp = Num of int
	      | Plus of exp * exp
	      | Minus of exp * exp
type formula = True
	      | False
	      | Not of formula
	      | AndAlso of formula * formula
	      | OrElse of formula * formula
	      | Imply of formula * formula
	      | Equal of exp * exp


let rec eval e=
      match e with 
      | True -> true
      | False -> false
      | Not(a) -> if eval(a) = true then false
                         else true
      | AndAlso(a,b) -> eval(a)&&eval(b) 
      | OrElse(a,b) -> eval(a)||eval(b)
      | Imply(a,b) -> if eval(a)= false then true
                      else if eval(a)= true && eval(b)=true then true
                      else false
      |Equal(a,b) -> let rec calExp f =
                     match f with 
                       |Num(a) -> a
                       |Plus(a,b) -> calExp(a)+calExp(b)
                       |Minus(a,b) -> calExp(a)-calExp(b)
            in if calExp(a) = calExp(b) then true
                  else false

      