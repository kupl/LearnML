exception Input_out_of_domain_of_rule

let rec prime : int -> bool
= fun n -> let n = abs n in
	if (n < 2) then false else (let rec sub_prime a = if(a == 1) then true else (if((a == 1) || (n mod a) == 0) then false else sub_prime (a-1)) in sub_prime (n-1)) (* TODO *)
