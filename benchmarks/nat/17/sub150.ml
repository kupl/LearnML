type nat =
    | ZERO
    | SUCC of nat

let rec natadd (na, nb) =
    match na with
    | ZERO -> nb
    | SUCC x -> natadd(x, (SUCC nb))

let rec natmul(na, nb) =
    match na with
    | ZERO -> ZERO
    | SUCC x -> natadd(nb, natmul(x, nb))
