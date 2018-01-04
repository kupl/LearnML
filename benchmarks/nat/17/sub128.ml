type nat = ZERO | SUCC of nat

let rec natadd ((a:nat), (b:nat)) : nat =
    match (a, b) with
    | (_, ZERO) -> a
    | (ZERO, _) -> b
    | (SUCC c, _) -> natadd(c, SUCC b)

let rec natmul ((a:nat), (b:nat)) : nat = 
    match (a, b) with
    | (_, ZERO) -> ZERO
    | (ZERO, _) -> ZERO
    | (SUCC ZERO, _) -> b
    | (_, SUCC ZERO) -> a
    | (SUCC c, _) -> natadd(natmul(c, b), b)
