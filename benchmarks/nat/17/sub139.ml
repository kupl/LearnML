type nat = ZERO | SUCC of nat

let rec natadd (a, b) : nat = 
    match (a, b) with
    | (ZERO, _) -> b
    | (SUCC p, _) -> natadd (p, SUCC b)

let rec natmul (a, b) : nat = 
    match (a, b) with
    | (ZERO, _) -> ZERO
    | (SUCC p, _) -> natadd(natmul(p, b), b)


