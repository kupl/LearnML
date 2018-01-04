type nat = ZERO | SUCC of nat

exception NoPredecessor

let pred n =
  match n with
  |ZERO -> raise NoPredecessor
  |SUCC(m) -> m


let rec natadd (nat1, nat2) = 
  match nat1 with
  |ZERO -> nat2
  |SUCC(m) -> SUCC(natadd (pred nat1, nat2))


let rec natmul (nat1, nat2) =
  match nat1 with
  |ZERO -> ZERO
  |SUCC(m) -> natadd(nat2, natmul(pred nat1, nat2))


