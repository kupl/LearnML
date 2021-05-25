type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> let rec suc = fun k -> match k with
             | ZERO -> ZERO
             | SUCC a -> SUCC (suc a)
             in suc n1
| SUCC b -> SUCC(natadd n1 b )

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC a -> natadd n1 (natmul n1 a)
