type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
    match (a, b) with
    | (ZERO, _) -> b
    | (_, ZERO) -> a
    | (SUCC x, SUCC y) -> natadd (SUCC a, y)

let rec natmul (a, b) =
    match (a, b) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC ZERO, _) -> b
    | (SUCC x, _) -> natadd (natmul (x, b), b)
