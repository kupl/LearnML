let rec check : int * int -> bool
= fun (n1, n2) ->
	if n1 = n2 then true
	else if n2 mod n1 = 0 then false
	else check(n1+1,n2);;	

let rec prime : int -> bool
= fun n -> (* TODO *)
	check(2, n);;
