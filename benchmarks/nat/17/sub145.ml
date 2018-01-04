type nat = ZERO | SUCC of nat;;

let rec natadd ((a: nat), (b: nat)) : nat =
    match (a, b) with
    | (ZERO, ZERO) -> ZERO
    | (ZERO, _) -> b
    | (_, ZERO) -> a
    | (SUCC rest, _) -> (natadd (rest, SUCC b));;

let rec natmul ((a: nat), (b: nat)) : nat =
    match (a, b) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC ZERO, _) -> b
    | (_, SUCC ZERO) -> a
    | (SUCC rest, _) -> natadd((natmul (rest, b)), b)
