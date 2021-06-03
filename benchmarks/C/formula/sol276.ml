type formula =
  True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = 
  Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec ecal e =
  match e with
    Num i -> i
  | Plus (i, j) -> (ecal i) + (ecal j)
  | Minus (i, j) -> (ecal i) - (ecal j)  

let rec eval p =
  match p with
    True -> true
  | False -> false
  | Not p -> 
	if (eval p) == true then false else true
  | AndAlso (p, q) -> 
	if (((eval p) == true) && ((eval q) == true)) then true else false
  | OrElse (p, q) ->
	if (((eval p) == false) && ((eval q) == false)) then false else true
  | Imply (p, q) ->
	if (((eval p) == true) && ((eval q) == false)) then false else true
  | Equal (e1, e2) ->
	if ((ecal e1) = (ecal e2)) then true else false


