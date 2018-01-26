let rec div : int * int -> bool  
= fun (n1, n2) ->
	if n2 = 1 then true
	else if (n1 mod n2) > 0 then div(n1,n2-1)
	else false
let rec prime : int -> bool
= fun n ->
	if n = 1 then false
	else div(n , n-1)
