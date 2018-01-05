
(* ex 6 *)

type nat = ZERO | SUCC of nat

let rec natadd(n1,n2) =
	match (n1,n2) with
	| (ZERO,ZERO) -> ZERO
	| (ZERO,b) -> natadd(b,ZERO)
	| (a,ZERO) -> a
	| (a,(SUCC b)) -> natadd((SUCC a),b)

let natmul(n1,n2) = 
	let rec cal_mul(a,b) =
		match (a,b) with
		| (_,ZERO) -> ZERO 
		| (x,(SUCC y)) -> natadd(x,cal_mul(x,y))
	in

	match (n1,n2) with
	| (ZERO,_) -> ZERO
	| (_,ZERO) -> ZERO
	| (a,b) -> cal_mul(a,b)


