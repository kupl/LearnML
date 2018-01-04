type nat = ZERO
| SUCC of nat

let rec natadd (a,b) =
match a with
| ZERO -> if b=ZERO then ZERO else natadd(b,a)
| SUCC(a) -> SUCC(natadd(a,b))

let rec natmul (a,b) =
match a with
| ZERO -> ZERO
| SUCC(a) -> natadd(natadd(ZERO,b),natmul(a,b))


