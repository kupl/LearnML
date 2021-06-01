type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and
exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval f =
  let rec cal_exp exp =
    match exp with
    | Num i -> i
    | Plus(e1,e2) -> (cal_exp e1) + (cal_exp e2)
    | Minus(e1,e2) -> (cal_exp e1) - (cal_exp e2)
  in
  match f with
  | True -> true
  | False -> false
  | Not f' -> not (eval f')
  | AndAlso(f1,f2) -> (eval f1) && (eval f2)
  | OrElse(f1,f2) -> (eval f1) || (eval f2)
  | Imply(f1,f2) -> if ((eval f1) = true) & ((eval f2) = false) then false else true
  | Equal(e1,e2) -> if (cal_exp e1) = (cal_exp e2) then true else false

  (*
let e1 = Plus(Minus(Plus(Plus(Num 1,Num 2),Num 3), Num 4), Num 5)(* 7*)
let e2 = Minus(Plus(Minus(Plus(Plus(Num 1,Num 2),Num 3), Num 4), Num 5),Num 2)
(* 5*)

let f1 = Equal(e1,e2) (* false *)
let f2 = Not f1 (* true *)
let f3 = AndAlso(f1, f2) (* false *)
let f4 = OrElse(f2,f3) (* true *)
let f5 = Imply(f3,f4) (* true *)
*)
