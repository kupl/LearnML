exception Input_out_of_domain_of_rule
let rec pascal : int * int -> int
= fun (n1, n2) -> if (n1 < 0 || n2 < 0 || n1 < n2) then raise Input_out_of_domain_of_rule else
	if (n1 == n2 || n2 == 0) then 1 else (pascal (n1-1, n2-1)) + (pascal (n1-1, n2)) (* TODO *)
