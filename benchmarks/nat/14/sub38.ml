type nat =
    | ZERO
    | SUCC of nat

let rec natadd ((n1: nat), (n2: nat)): nat =
    match n1 with
    | ZERO -> (match n2 with
               | ZERO -> ZERO
               | SUCC n -> n2
               )
    | SUCC n -> SUCC (natadd (n, n2))


let rec natmul ((n1: nat), (n2: nat)): nat =
    match (n1, n2) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC n1_, SUCC n2_) -> natadd (n1, (natmul (n1, n2_)))
