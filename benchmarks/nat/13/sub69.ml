type nat = ZERO | SUCC of nat

let rec natadd(a,b) =
 match (a,b) with
 | (ZERO,y) -> y
 | (SUCC x, y) -> natadd(x,SUCC y)

let rec natmul(a,b) =
 match (a,b) with
 | (ZERO,y) -> ZERO
 | (x, ZERO) -> ZERO
 | (SUCC x, y) -> natadd(y,natmul(x,y))