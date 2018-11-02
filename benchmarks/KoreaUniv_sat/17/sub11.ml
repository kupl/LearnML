(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f -> 
let rec funsat x = match x with
| True -> True
| False -> False
| Var str -> if str = "P" then (funsat True) else if str = "Q" then funsat False else raise (Failure "input error")
| Neg f1 -> if (funsat f1 = True) then False else if (funsat f1 = False) then True else raise (Failure "bool error")
| And (f1, f2) -> if (funsat f1 = True && funsat f2 = True) then True else False
| Or (f1, f2) -> if (funsat f1 = True || funsat f2 = True) then True else False
| Imply (f1, f2) -> if (funsat f1 = True) then funsat f2 else True
| Iff (f1, f2) -> if (f1 = f2) then True else False 
in if (funsat f = True) then true else false 