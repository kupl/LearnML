(* 
2011-10634
JooHyun Jo / Major in Economics
problem 1 for HW2
*)

type formula = True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec expToInt (e:exp):int = 
  match e with
  | Num(i) -> i
  | Plus((e1),(e2)) -> expToInt(e1) + expToInt(e2)
  | Minus((e1),(e2)) -> expToInt(e1) - expToInt(e2)

let rec eval (f:formula) :bool =
  match f with
  | True -> true
  | False -> false
  | Not (negated) -> not (eval(negated))
  | AndAlso((f1), (f2)) -> (eval(f1)) && (eval(f2))
  | OrElse((f1), (f2)) -> (eval(f1)) || (eval(f2))
  | Imply ((f1), (f2)) -> (not (eval(f1))) || (eval(f2))
  | Equal ((e1),(e2)) -> expToInt(e1) = expToInt(e2)

