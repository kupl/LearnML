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

let flip : bool -> bool
= fun n -> match n with
  | true -> false
  | false -> true;;

let rec expeval : exp -> int
= fun n -> match n with
  | Num x -> x
  | Plus (x, y) -> (expeval x) + (expeval y)
  | Minus (x, y) -> (expeval x) - (expeval y);;
  
let rec expequal : exp -> exp -> bool
= fun n m -> if (expeval n) = (expeval m) then true
else false;;

let rec eval : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Not x -> flip (eval x)
  | AndAlso (x, y) -> (eval x) && (eval y)
  | OrElse (x, y) -> (eval x) || (eval y)
  | Imply (x, y) -> (flip (eval x)) || (eval y)
  | Equal (x, y) -> expequal x y;;



