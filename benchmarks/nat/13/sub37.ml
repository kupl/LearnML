type nat = ZERO | SUCC of nat


let rec calc x =
    match x with
    | ZERO -> 0
    | SUCC a -> (calc a) + 1

let rec recover x =
    if x = 0 then ZERO
    else SUCC (recover (x-1))

let natadd (n1, n2) =
    recover ((calc n1) + (calc n2))

let natmul (n1, n2) =
    recover ((calc n1) * (calc n2))
