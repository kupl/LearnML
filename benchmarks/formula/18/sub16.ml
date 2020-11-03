type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  
let rec calc : exp -> int
= fun e -> match e with
  Num n -> n
  |Plus (n1, n2) -> calc(n1) + calc(n2)
  |Minus (n1, n2) -> calc(n1) - calc(n2);;
  
let rec eval : formula -> bool
= fun f -> match f with
  True -> true
  |False -> false
  |Not b1 -> if eval(b1) then false else true
  |AndAlso (b1, b2) -> eval(b1) && eval(b2)
  |OrElse (b1, b2) -> eval(b1) || eval(b2)
  |Imply (b1, b2) -> if (eval(b1) = false) && (eval(b2) = true) then false else true
  |Equal (e1, e2) -> calc(e1) = calc(e2);;