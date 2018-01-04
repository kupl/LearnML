(* HW1 Exercise 4 Natural Numbers *)

type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a, b) ->
    match a with
    | ZERO -> b
    | SUCC n ->
        (match b with
        | ZERO -> a
        | SUCC m -> natadd (n, SUCC b)
        )

let rec natmul : nat * nat -> nat = fun (a, b) ->
    match a with
    | ZERO -> ZERO
    | SUCC n ->
        (match b with
        | ZERO -> ZERO
        | SUCC m -> natadd ((natmul (a, m)), a)
        )
