(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
;;

let rec evalSigma(lo, hi, op) =
if lo > hi then 0
else
let flag = (if lo < hi then 1 else 0) in
match op with
  | X -> lo + flag * evalSigma(lo+1, hi, op)
  | INT c -> c + flag * evalSigma(lo+1, hi, op)
  | ADD (x, y) -> evalSigma(lo, lo, x) + evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | SUB (x, y) -> evalSigma(lo, lo, x) - evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | MUL (x, y) -> evalSigma(lo, lo, x) * evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | DIV (x, y) -> evalSigma(lo, lo, x) / evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | SIGMA(INT st, INT ed, x) -> evalSigma(st, ed, x) + flag * evalSigma(lo+1, hi, op)
  | _ -> 0;;


  let calculator : exp -> int
  = fun exp -> match exp with
  | INT x -> x
  | ADD (INT x, INT y) -> x + y
  | SUB (INT x, INT y) -> x - y
  | MUL (INT x, INT y) -> x * y
  | DIV (INT x, INT y) -> x / y
  | SIGMA (INT lo, INT hi, op) -> evalSigma(lo, hi, op)
  | _ -> raise NotImplemented;;