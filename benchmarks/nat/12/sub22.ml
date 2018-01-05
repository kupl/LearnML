(* Ex7 *)
type nat = ZERO | SUCC of nat
let rec natadd(nat1, nat2) =
   match (nat1, nat2) with
   | (ZERO, n) -> n
   | (n, ZERO) -> n
   | (SUCC n1, SUCC n2) -> natadd(n1, SUCC(SUCC n2))
let rec natmul(nat1, nat2) =
   match (nat1, nat2) with
   | (ZERO, _) -> ZERO
   | (_, ZERO) -> ZERO
   | (SUCC n1, SUCC n2) -> natadd(SUCC n2, natmul(n1, SUCC n2))
