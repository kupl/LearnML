(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
True -> true
|False -> false
|Neg f -> eval f
|Or (f,g) -> if (eval f || eval g) then true else false
|And (f,g) -> if(eval f && eval g) then true else false
|Imply (f,g) -> if(eval f) then eval g else true
|Equiv (f,g) -> if(eval f == eval g) then true else false;;


