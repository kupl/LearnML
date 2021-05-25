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

let rec eval p =
    let rec evalexp e = match e with
		Num n -> n
	  | Plus (e1, e2) -> (evalexp e1) + (evalexp e2)
	  | Minus (e1, e2) -> (evalexp e1) - (evalexp e2)
	in

    match p with
    True -> true
  | False -> false
  | Not p1 -> if eval p1 then false else true
  | AndAlso (p1, p2) -> if eval p1 then if eval p2 then true else false else false
  | OrElse (p1, p2) -> if eval p1 then true else if eval p2 then true else false
  | Imply (p1, p2) -> if eval p1 then if eval p2 then true else false else true
  | Equal (e1, e2) -> (evalexp e1) = (evalexp e2)