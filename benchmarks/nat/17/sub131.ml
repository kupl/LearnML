(* real code start*)
type nat = ZERO | SUCC of nat

let rec natadd ((a: nat),(b: nat)) : nat =
 match a with
| ZERO -> b
| SUCC(x) -> natadd(x, SUCC(b))

let rec natmul ((a: nat),(b: nat)) : nat =
 match a with 
| ZERO -> ZERO
| SUCC(x) -> natadd(b, natmul(x,b))
(*real code end*)
