type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat = fun (a, b) ->
    match a with
    | ZERO -> b
    | SUCC n -> natadd (n, SUCC b)

let rec natmul: nat * nat -> nat = fun (a, b) ->
    match b with
    | ZERO -> ZERO
    | SUCC n -> natadd (natmul(a, n), a)
