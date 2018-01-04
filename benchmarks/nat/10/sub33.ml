type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
 match (nat1, nat2) with
 (ZERO,_) -> nat2
 |(_,ZERO) -> nat1
 |(SUCC s_nat1, SUCC s_nat2) -> (natadd (s_nat1, SUCC (nat2)))


let rec natmul (nat1, nat2) =
 match (nat1, nat2) with
 (ZERO,_) -> ZERO
 |(SUCC ZERO, _) -> nat2
 |(_, ZERO) -> ZERO
 |(_, SUCC ZERO) -> nat1
 |(SUCC s_nat1, SUCC s_nat2) -> (natadd (nat1,(natmul (nat1, s_nat2))))

