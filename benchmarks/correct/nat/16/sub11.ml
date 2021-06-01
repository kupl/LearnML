type nat =
	| ZERO
	| SUCC of nat

let rec count n =
	match n with
	| ZERO -> 0 
	| SUCC (n1) -> 1 + count n1

let rec make : int -> nat -> nat 
= fun n1 n2 -> if n1 = 0 then n2 else let n2 = SUCC (n2) in make (n1-1) n2

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> n1
	| SUCC (n) -> let n1 = SUCC (n1) in natadd n1 n

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	let n11 = count n1 in
		let n22 = count n2 in
			natadd ZERO (make (n11*n22) ZERO)
