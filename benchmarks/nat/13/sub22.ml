type nat = ZERO
| SUCC of nat



let rec natadd (a,b) =
match (a,b) with
| (ZERO, b) -> b
| (a, ZERO) -> a
| (SUCC a, SUCC b) -> natadd(SUCC (SUCC a), b)

let rec natmul (a,b) =
match (a,b) with
| (ZERO, b) -> ZERO
| (a, ZERO) -> ZERO
| (a, SUCC n) -> natadd(a, natmul(a,n))
