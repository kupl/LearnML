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

let rec eval (x: formula): bool =
  match x with
  | True -> true
  | False -> false
  | Not(f) -> not(eval f)
  | AndAlso(g, h) -> (eval g) && (eval h)
  | OrElse(i, j) -> (eval i) || (eval j)
  | Imply(k, l) -> if (eval k) then (eval l)
                                      else true
  | Equal(c, d) -> 
      let rec calculator (inputExpr : exp) : int =
        match inputExpr with
        | Num(i) -> i
        | Plus(e1, e2) -> calculator(e1) + calculator(e2)
        | Minus(e3, e4) -> calculator(e3) - calculator(e4)
      in
      calculator(c) = calculator(d)
