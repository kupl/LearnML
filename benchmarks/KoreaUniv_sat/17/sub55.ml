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
let rec testa f = 
match f with
| True -> True
| False -> False
| Var _ -> f
| Neg(a) -> (match a with
  | True -> False
  | False -> True
  | Var _ -> Neg(a)
  | _ -> Neg(testa a) )
| And(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
    | True, True -> True
    | True, False -> False
    | True, f2 -> f2
    | False, _ -> False
    | f1, True -> f1
    | f1, False -> False
    | f1, f2 -> if f2 = Neg(f1) then False 
    else f1)
| Or(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
 | True, _ -> True
 | False, True -> True
 | False, False -> False
 | False, f2 -> f2
 | f1, True -> True
 | f1, False -> f1
 | f1, f2 -> if f2 = Neg(f1) then True
 else f1)
| Imply(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
 | True, True -> True
 | True, False -> False
 | True, f2 -> f2
 | False, _ -> True
 | f1, True -> True
 | f1, False -> f1
 | f1, f2 -> if f2 = f1 then True
 else f1)
| Iff(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
 | True, True -> True
 | True, False -> False
 | True, f2 -> f2
 | False, True -> False
 | False, False -> True
 | False, f2 -> f2
 | f1, True -> f1
 | f1, False -> f1
 | f1, f2 -> if f2 = f1 then True else if f2 = Neg(f1) then False 
 else f1);;

let sat : formula -> bool
= fun f ->
match f with
| True | False | Var _ -> false
| _ -> 
let a = testa f in
(match a with
 | True | False -> false
 | _ -> true);;
