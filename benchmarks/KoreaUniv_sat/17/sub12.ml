(* problem 3 *)
type formula =
| True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let sat : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Var str -> true (*raise (Failure "type error")*)
| Neg formula -> if formula = True then false else true
| And (f1, f2) -> if f1 = Neg f2 then false
                  else if f2 = Neg f1 then false
                  else true
| Or (f1, f2) -> if f1 = f2 && f1 = False then false
                 else true
| Imply (f1, f2) -> if f1 <> f2 && f1 = True then false
                    else true
| Iff (f1, f2) -> if f1 = Neg f2 || f2 = Neg f1 then false
                  else true;;
