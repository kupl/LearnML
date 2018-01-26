let rec prime_loop : int * int -> bool
= fun (n1, n2) ->
	if n1 mod n2 = 0 && n2 != 1 then
		false
	else if n2>1 then
		prime_loop(n1,n2-1)
	else
		true

			
(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
if n = 2 then true 
else prime_loop(n,n-1)
 (* TODO *)