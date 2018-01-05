type nat = ZERO | SUCC of nat

let natadd(x, y) =
	let rec count t n =
	match t with
	SUCC(t1) -> count t1 (n+1)
	|ZERO -> n
	in

	let rec resolve n =
	if n = 0 then ZERO
	else SUCC(resolve (n-1))
	in

	resolve ((count x 0) + (count y 0))

let natmul(x, y) =
	let rec count t n =
	match t with
	SUCC(t1) -> count t1 (n+1)
	|ZERO -> n
	in

	let rec resolve n =
	if n = 0 then ZERO
	else SUCC(resolve (n-1))
	in

	resolve ((count x 0) * (count y 0))
