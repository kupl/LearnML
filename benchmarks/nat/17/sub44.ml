type nat = ZERO
		 | SUCC of nat

let rec natadd : nat*nat -> nat = fun(x, y) ->
	match x,y with
	| ZERO, _ -> y
	| _, ZERO -> x
	| SUCC(xt), SUCC(yt) -> natadd(SUCC(x), yt)

let natmul : nat*nat -> nat = fun(x, y) ->
	match x,y with
	| ZERO, _ -> ZERO
	| _, ZERO -> ZERO
	| SUCC(xt), SUCC(yt) -> 
	(
		let rec help : nat*nat*nat -> nat = fun(a, b, result) ->
			match b with
			| ZERO -> result
			| SUCC(bt) -> help(a, bt, natadd(a, result))
		in
		help(x, y, ZERO)
	)



(*
let rec print_nat : nat->unit = fun x ->
	match x with
	| ZERO -> print_string("ZERO")
	| SUCC(s) -> (  print_string("SUCC(");
					print_nat(s);
					print_string(")");
				 )

let rec nat_to_int : nat->int = fun x ->
	match x with
	| ZERO -> 0
	| SUCC(xt) -> 1 + nat_to_int(xt)

let rec int_to_nat : int->nat = fun x ->
	match x with
	| 0 -> ZERO
	| _ -> SUCC(int_to_nat(x-1))

let _ = print_nat (SUCC(ZERO))
let _ = print_newline()
let _ = print_int(nat_to_int(natadd(int_to_nat(100), int_to_nat(12))))
let _ = print_newline()
let _ = print_int(nat_to_int(natmul(int_to_nat(2222), int_to_nat(10))))
let _ = print_newline()


let _ = 
let rec nat_to_int : nat -> int = 
fun n -> 
match n with 
| ZERO -> 0 
| SUCC n1 -> 1 + nat_to_int n1 
in 

let print_bool x = 
print_endline (string_of_bool x) 
	in 

	let three = SUCC (SUCC (SUCC ZERO)) 
	in 
	let four = SUCC three 
	in 

	print_bool (7 = nat_to_int (natadd (three, four))); 
	print_bool (0 = nat_to_int (natadd (ZERO, ZERO))); 
	print_bool (3 = nat_to_int (natadd (ZERO, three))); 
	print_bool (4 = nat_to_int (natadd (four, ZERO))); 

	print_bool (12 = nat_to_int (natmul (three, four))); 
	print_bool (0 = nat_to_int (natmul (ZERO, three))); 
	print_bool (0 = nat_to_int (natmul (four, ZERO))); 
	print_bool (0 = nat_to_int (natmul (ZERO, ZERO))); 
	print_bool (3 = nat_to_int (natmul (SUCC ZERO, three))); 
	print_bool (4 = nat_to_int (natmul (four, SUCC ZERO))); 
*)
