type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun n ->
    match n with
    |(ZERO, b) -> b
    |(SUCC(a), b) -> SUCC(natadd(a,b))

let rec natmul : nat * nat -> nat = fun n ->
    match n with
    |(ZERO, b) -> ZERO
    |(SUCC(a), b) -> natadd(natmul(a,b),b)