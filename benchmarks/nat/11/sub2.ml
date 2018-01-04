
type nat = ZERO | SUCC of nat
	
let rec natadd (n1,n2) = 
	let 
		rec nat_count (n_input) =
			match n_input with
			| ZERO -> 0
			| SUCC(n) -> 1+ nat_count(n)
	and
		nat_output (n_count) =
			if ( n_count > 0 ) then SUCC(nat_output(n_count-1))
			else ZERO
	in
		nat_output(nat_count(n1)+nat_count(n2))

let rec natmul (n1,n2) = 
	let 
		rec nat_count (n_input) =
			match n_input with
			| ZERO -> 0
			| SUCC(n) -> 1+ nat_count(n)
	and
		nat_output (n_count) =
			if ( n_count > 0 ) then SUCC(nat_output(n_count-1))
			else ZERO
	in
		nat_output(nat_count(n1)*nat_count(n2))
