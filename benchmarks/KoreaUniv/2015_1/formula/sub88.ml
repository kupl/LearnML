type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> true 

let rec eval s =
	match s with
	True -> true
	|False -> false
	|Neg f -> not (eval f)
	|Or (f, ff) ->(eval f) || (eval ff)
  |And (f, ff) ->(eval f) && (eval ff)
  |Imply (f , ff) -> not (not (eval f)  && (not (eval ff)))
	|Equiv (f , ff) -> (eval f)=(eval ff);;

