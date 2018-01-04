(* let ZERO = 0;; *)
(* let SUCC x = x + 2 *)

type nat =
ZERO
| SUCC of nat

let rec natadd_sub (a, n, k) = if n == k then a else natadd_sub (SUCC a, SUCC n, k);;

let natadd (a, b) = natadd_sub (a, ZERO, b);;

let rec natmul_sub (a, n, k) = if n == k then a else natmul_sub ( natadd(a, a), SUCC n, k );;

let natmul (a, b) = natmul_sub (a, SUCC(ZERO), b);;