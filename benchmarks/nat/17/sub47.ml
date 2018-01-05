type nat = ZERO | SUCC of nat

let rec natadd ((nat1 : nat), (nat2 : nat)) : nat =
    match (nat1, nat2) with
    | (ZERO, _) -> nat2
    | (_, ZERO) -> nat1
    | (SUCC _nat1, SUCC _nat2) -> SUCC (SUCC (natadd (_nat1, _nat2)))

let rec natmul ((nat1 : nat), (nat2 : nat)) : nat =
    match (nat1, nat2) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC _nat1, SUCC _nat2) -> natadd (nat1, natmul(nat1, _nat2))
