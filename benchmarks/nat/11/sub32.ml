type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
	match x with
	ZERO -> y
	| SUCC xx -> natadd(xx, SUCC y)

let rec natmul (x, y) =
	let rec natad (x, y) =
		match x with
		ZERO -> y
		| SUCC xx -> natad (xx, SUCC y)
	in
	match x with
	ZERO -> ZERO
	| SUCC xx -> natad(y, natmul(xx, y))
