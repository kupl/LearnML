(* 2009-11824 Jieun-Jeong HW1-6 *)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	let rec nat_to_int n = 
		match n with
		ZERO	-> 0
		|SUCC x	-> (nat_to_int x) + 1
	in
	let rec int_to_nat n =
		if n == 0 then ZERO
		else SUCC(int_to_nat (n-1))
	in
	int_to_nat((nat_to_int a) + (nat_to_int b))


let rec natmul (a, b) =
	let rec nat_to_int n = 
		match n with
		ZERO	-> 0
		|SUCC x	-> (nat_to_int x) + 1
	in
	let rec int_to_nat n =
		if n == 0 then ZERO
		else SUCC(int_to_nat (n-1))
	in
	int_to_nat((nat_to_int a) * (nat_to_int b))


