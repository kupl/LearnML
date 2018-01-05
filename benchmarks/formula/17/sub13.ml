(* 
2011-10634
JooHyun Jo / Major in Economics
problem 1 for HW2
*)

type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr

and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec exprToInt (e:expr):int = 
  match e with
  | NUM(i) -> i
  | PLUS((e1:expr),(e2:expr)) -> exprToInt(e1) + exprToInt(e2)
  | MINUS((e1:expr),(e2:expr)) -> exprToInt(e1) - exprToInt(e2)

let rec eval (f:formula) :bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT (negated:formula) -> not (eval(negated))
  | ANDALSO((f1:formula), (f2:formula)) -> (eval(f1)) && (eval(f2))
  | ORELSE((f1:formula), (f2:formula)) -> (eval(f1)) || (eval(f2))
  | IMPLY ((f1:formula), (f2:formula)) -> (not (eval(f1))) || (eval(f2))
  | LESS ((e1:expr),(e2:expr)) -> exprToInt(e1) < exprToInt(e2)

