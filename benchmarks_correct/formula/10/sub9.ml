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
let imply b1 b2 =
 if (b1 && b2) = true then true
 else
   if b1 = false then true
   else false

let rec evalex exp =
    match exp with
      Num(no) -> no
    | Plus(e1, e2) -> (evalex e1) + (evalex e2)
    | Minus(e1, e2) -> (evalex e1) - (evalex e2)

let rec eval formula =
    match formula with
      True -> true
    | False -> false
    | Not(f) -> not (eval f)
    | AndAlso(f1, f2) -> (eval f1) && (eval f2)
    | OrElse(f1, f2) -> (eval f1) || (eval f2)
    | Imply(f1, f2) -> imply (eval f1) (eval f2)
    | Equal(e1, e2) -> (evalex e1) = (evalex e2)




