type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let crazy2val crazy2 =
	let rec calc x n =
		match x with
		| NIL -> 0
		| ZERO a -> calc a (n+1)
		| ONE a -> power 2 n + calc a (n+1)
		| MONE a -> - power 2 n + calc a (n+1)
	and power x y =
		if y = 0 then 1
		else x * power x (y-1) in
	calc crazy2 0;;
