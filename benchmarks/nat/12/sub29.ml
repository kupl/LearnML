type nat = ZERO | SUCC of nat

let rec natadd (nat1,nat2) =
  match nat1 with
  | ZERO -> nat2
  | SUCC(n) -> SUCC(natadd(n,nat2))

let rec natmul (nat1,nat2) = 
  match nat1 with
  | ZERO -> ZERO
  | SUCC(n) -> natadd(nat2, natmul(n, nat2))

  (*
let a = SUCC(SUCC(SUCC(SUCC ZERO)))
let b = SUCC(SUCC(SUCC ZERO))
*)

