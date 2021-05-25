type nat =
	| ZERO
	| SUCC of nat

let rec iton n =
	match n with
	| ZERO -> 0
	| SUCC tl -> 1+iton tl

let rec comp i n = if (iton n)=i then n else comp i (SUCC (n))

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let v1 = iton n1 in
	let v2 = iton n2 in
	comp (v1+v2) ZERO

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let v1 = iton n1 in
	let v2 = iton n2 in
	comp (v1*v2) ZERO
