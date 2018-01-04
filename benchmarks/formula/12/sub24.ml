type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and
expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval f =
  let rec cal_exp exp =
    match exp with
    | NUM i -> i
    | PLUS(e1,e2) -> (cal_exp e1) + (cal_exp e2)
    | MINUS(e1,e2) -> (cal_exp e1) - (cal_exp e2)
  in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f' -> not (eval f')
  | ANDALSO(f1,f2) -> (eval f1) && (eval f2)
  | ORELSE(f1,f2) -> (eval f1) || (eval f2)
  | IMPLY(f1,f2) -> if ((eval f1) = true) & ((eval f2) = false) then false else true
  | LESS(e1,e2) -> if (cal_exp e1) < (cal_exp e2) then true else false

  (*
let e1 = PLUS(MINUS(PLUS(PLUS(NUM 1,NUM 2),NUM 3), NUM 4), NUM 5)(* 7*)
let e2 = MINUS(PLUS(MINUS(PLUS(PLUS(NUM 1,NUM 2),NUM 3), NUM 4), NUM 5),NUM 2)
(* 5*)

let f1 = LESS(e1,e2) (* false *)
let f2 = NOT f1 (* true *)
let f3 = ANDALSO(f1, f2) (* false *)
let f4 = ORELSE(f2,f3) (* true *)
let f5 = IMPLY(f3,f4) (* true *)
*)
