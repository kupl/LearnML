(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 1-4 *)
type nat = ZERO | SUCC of nat
let rec natadd : nat * nat -> nat = fun (a,b) ->
    match b with
    | ZERO -> a
    | SUCC b' -> SUCC (natadd (a, b'))
let rec natmul : nat * nat -> nat = fun (a,b) ->
    match b with
    | ZERO -> ZERO
    | SUCC ZERO -> a
    | SUCC b' -> natadd(a, natmul(a, b'))
