(* 2011210039 Kang Seungwoo *)

exception Problem;;

type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval f =
match f with
True -> true
| False -> false
| Neg s -> not (eval s)
| Or (s, t) -> (eval s) || (eval t)
| And (s, t) -> (eval s) && (eval t)
| Imply (s, t) -> (not (eval s)) || (eval t)
| Equiv (s, t) -> (eval s) = (eval t);;

