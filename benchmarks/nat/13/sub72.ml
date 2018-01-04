
type nat = ZERO | SUCC of nat


let nat_counter n = 
	let rec nat_counter_inner n res = 
		match n with
		| ZERO -> res
		| SUCC(nat) -> nat_counter_inner nat (res+1)
	in
	nat_counter_inner n 0 

let nat_generator num = 
	let rec nat_generator_inner num res = 
		if num=0 then res
		else nat_generator_inner (num-1) (SUCC(res))
	in
	nat_generator_inner num ZERO

let natadd = fun (n1, n2) -> 
	let n1_res = nat_counter n1 in
	let n2_res = nat_counter n2 in
	nat_generator (n1_res+n2_res)

let natmul = fun (n1, n2) -> 
	let n1_res = nat_counter n1 in
	let n2_res = nat_counter n2 in
	nat_generator (n1_res*n2_res)
