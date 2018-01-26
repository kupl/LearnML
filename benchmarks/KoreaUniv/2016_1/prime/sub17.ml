exception Illegal_input

(* Problem 3 *)
let rec check : int * int -> bool
= fun (n, p) ->
	if p*p>n then true
	else if n mod p = 0 then false
	else check (n, p+1)

let rec prime : int -> bool
= fun n -> (* TODO *)
	if n<=0 then raise Illegal_input
	else if n=1 then false
	else check (n, 2)
