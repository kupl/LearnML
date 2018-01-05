type nat = ZERO | SUCC of nat

let rec natadd e =
match e with 
|ZERO, SUCC(b) -> SUCC(b)
| SUCC(a),ZERO -> SUCC(a)
| ZERO, ZERO -> ZERO
| SUCC(a),SUCC(b) -> natadd ((a), SUCC(SUCC(b)))



let rec natmul f =
match f with 
|ZERO, SUCC(b) -> ZERO
| SUCC(a),ZERO -> ZERO
|nZERO, ZERO -> ZERO
|SUCC(a),SUCC(b) -> natadd(natmul(a, SUCC(b)),SUCC(b))
