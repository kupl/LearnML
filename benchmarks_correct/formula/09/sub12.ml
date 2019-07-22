type expr = NUM of int
	      | PLUS of expr * expr
	      | MINUS of expr * expr
type formula = TRUE
	      | FALSE
	      | NOT of formula
	      | ANDALSO of formula * formula
	      | ORELSE of formula * formula
	      | IMPLY of formula * formula
	      | LESS of expr * expr


let rec eval e=
      match e with 
      | TRUE -> true
      | FALSE -> false
      | NOT(a) -> if eval(a) = true then false
                         else true
      | ANDALSO(a,b) -> eval(a)&&eval(b) 
      | ORELSE(a,b) -> eval(a)||eval(b)
      | IMPLY(a,b) -> if eval(a)= false then true
                      else if eval(a)= true && eval(b)=true then true
                      else false
      |LESS(a,b) -> let rec calExp f =
                     match f with 
                       |NUM(a) -> a
                       |PLUS(a,b) -> calExp(a)+calExp(b)
                       |MINUS(a,b) -> calExp(a)-calExp(b)
            in if calExp(a) < calExp(b) then true
                  else false

      