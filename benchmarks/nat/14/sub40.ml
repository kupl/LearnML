type nat = ZERO | SUCC of nat



let natadd : nat * nat -> nat =
	let rec change : nat -> int =
		fun x ->
			match x with
				|ZERO -> 0
				|SUCC a -> (1 + (change a)) in
	let rec callback : int -> nat =
		fun x ->
			if x > 0 then (SUCC (callback (x-1)))
			else ZERO in
	fun pair ->
		match pair with
			| (a, b) -> (callback ((change a) + (change b)))
let natmul : nat * nat -> nat =
	let rec change : nat -> int =
		fun x ->
			match x with
				|ZERO -> 0
				|SUCC a -> (1 + (change a)) in
	let rec callback : int -> nat =
		fun x ->
			if x > 0 then (SUCC (callback (x-1)))
			else ZERO in
	fun pair ->
		match pair with 
			| (a, b) -> (callback ((change a) * (change b)))