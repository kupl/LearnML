type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
= fun p ->
match p with
| True -> true
| False -> false
| Neg value -> not (eval value)
| Or (left, right) -> (eval left) || (eval right)
| And (left, right) -> (eval left) && (eval right)
| Imply (left, right) -> not (eval left) || (eval right)
| Equiv (left, right) -> (eval left) = (eval right);;


