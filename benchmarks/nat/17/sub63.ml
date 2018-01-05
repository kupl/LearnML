type nat = ZERO | SUCC of nat

let rec natadd (x : nat * nat ) : nat =
let (a, b) = x in
match b with
| ZERO -> a
| SUCC b' -> SUCC (natadd(a, b'))

let rec natmul ( x : nat * nat ) : nat =
let (a, b) = x in
match b with
| ZERO -> ZERO
| SUCC ZERO -> a
| SUCC b' -> natadd (a, natmul (a, b'))
