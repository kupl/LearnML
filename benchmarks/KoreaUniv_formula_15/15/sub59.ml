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
      | False -> false
      | Neg p -> if p=True then false else true
      | Or (p, q) -> if p=False && q=False then false else true
      | And (p, q) -> if p=True && q=True then true else false
      | Imply (p, q) -> if p=True && q=False then false else true
      | Equiv (p, q) -> if p=True && q=True then true
                        else if p=False && q=False then true
                        else false;;
