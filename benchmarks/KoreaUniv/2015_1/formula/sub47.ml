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
| Neg a -> not (eval a)
| Or (a, b) -> eval a || eval b
| And (a, b) -> eval a && eval b
| Imply (a, b) -> not (eval a) || eval b
| Equiv (a, b) -> eval a = eval b
