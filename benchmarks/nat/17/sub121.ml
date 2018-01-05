type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
    match b with
    | ZERO -> a
    | SUCC(b') -> natadd (SUCC(a), b')

let natmul (a, b) =
    let rec natmul' (a, b) acc = 
        match b with
        | ZERO -> acc
        | SUCC(b') -> natmul' (a, b') (natadd (acc, a))
    in
    natmul' (a, b) ZERO

