(* problem 3 *)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula
let rec find_x
= fun f -> match f with
  | True -> True
  | False -> False
  | Var(x) -> Var(x)
  | Neg(x) -> find_x x
  | And(x, y) -> find_x x
  | Or(x, y) -> find_x x
  | Imply(x, y) -> find_x x
  | Iff(x, y) -> find_x x
let rec find_y
= fun f -> match f with
  | True -> True
  | False -> False
  | Var(y) -> Var(y)
  | Neg(y) -> find_y y
  | And(x, y) -> find_y y
  | Or(x, y) -> find_y y
  | Imply(x, y) -> find_y y
  | Iff(x, y) -> find_y y
let rec same
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 1
  | Neg(x) -> if ((same x) = 1) then 0 else 1
  | And(x, y) -> (same x)*(same y)
  | Or(x, y) -> if ((same x) + (same y)) = 0 then 0 else 1(*((same x) + (same y))/2*)
  | Imply(x, y) -> if ((same x) = 1)&&((same y) = 0) then 0 else 1
  | Iff(x, y) -> if((same x) = (same y)) then 1 else 0
let rec same_f
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 0
  | Neg(x) -> if ((same_f x) = 1) then 0 else 1
  | And(x, y) -> (same_f x)*(same_f y)
  | Or(x, y) -> if ((same_f x) + (same_f y)) = 0 then 0 else 1
  | Imply(x, y) -> if ((same_f x) = 1)&&((same_f y) = 0) then 0 else 1
  | Iff(x, y) -> if ((same_f x) = (same_f y)) then 1 else 0
let rec dif
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 1
  | Neg(x) -> if ((dif x) = 1) then 0 else 1
  | And(x, y) -> (dif x)*(same_f y)
  | Or(x, y) -> if ((dif x) + (same_f y)) = 0 then 0 else 1
  | Imply(x, y) -> if ((dif x) = 1)&&((same_f y) = 0) then 0 else 1
  | Iff(x, y) -> if ((dif x) = (same_f y)) then 1 else 0
let rec dif_f
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 0
  | Neg(x) -> if ((dif_f x) = 1) then 0 else 1
  | And(x, y) -> (dif_f x)*(same y)
  | Or(x, y) -> if ((dif_f x) + (same y)) = 0 then 0 else 1
  | Imply(x, y) -> if ((dif_f x) = 1) && ((same y) = 0) then 0 else 1
  | Iff(x, y) -> if ((dif_f x) = (same y)) then 1 else 0
let check
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 1
  | _ -> if ((find_x f) = (find_y f)) then (same f) + (same_f f)
         else (same f) + (same_f f) + (dif f) + (dif_f f)
let sat : formula -> bool
= fun f -> match f with
  | _ -> if ((check f) = 0) then false else true
