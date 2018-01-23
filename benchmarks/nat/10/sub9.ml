type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
  match (a, b) with (ZERO, x) -> x
  | (x, ZERO) -> x
  | (SUCC c, d) -> SUCC (natadd (c, d))

let rec natmul (a, b) =
  match (a, b) with (ZERO, x) -> ZERO
  | (x, ZERO) -> ZERO
  | (c, SUCC d) -> natadd(c, natmul(c, d))

(* let _ = print_nat( natadd (SUCC(SUCC(SUCC(SUCC(SUCC(ZERO))))), SUCC(SUCC(SUCC(ZERO)))) );; *)
