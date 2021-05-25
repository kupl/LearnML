
(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec add c1 c2 =
match c1 
with ZERO -> (match c2
	with ZERO -> ZERO 
	| SUCC(a) -> SUCC( add c1 a ))
| SUCC(b) -> SUCC( add b c2 )
in add n1 n2 

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let rec mul c2 = 
match c2 
with ZERO -> ZERO
| SUCC( a) -> natadd n1 (mul a) 
in mul n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec exp c2 =
match c2
with ZERO -> SUCC(ZERO)
| SUCC( a) -> natmul n1 (exp a)
in exp n2
