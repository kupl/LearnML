(*2006-11681 °­Çö¼®*)
type nat = ZERO
		 | SUCC of nat

let natadd(a,b) =
	
	let rec nat_to_num e =
		match e with
		ZERO -> 0
		| SUCC x -> 1+(nat_to_num x)
	in
	
	let rec num_to_nat n =
		match n with
		0 -> ZERO
		| _ -> (SUCC (num_to_nat (n-1)))
	in

	(num_to_nat ((nat_to_num a)+(nat_to_num b)))

let natmul(a,b) =
	
	let rec nat_to_num e =
		match e with
		ZERO -> 0
		| SUCC x -> 1+(nat_to_num x)
	in

	let rec num_to_nat n =
		match n with
		0 -> ZERO
		| _ -> (SUCC (num_to_nat (n-1)))
	in

		(num_to_nat ((nat_to_num a)*(nat_to_num b)))
