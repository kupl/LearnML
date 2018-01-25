(* Problem 1 *)
let rec fib : int -> int
= fun n -> (* TODO *) 
	if n = 1 then 1
	else if n = 2 then 1
	else fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> (* TODO *)
	if n2 = 0 || n2 = n1 then 1
	else pascal (n1-1,n2-1) + pascal (n1-1,n2);;

(* Problem 3 *)
let rec check : int * int -> bool
= fun (n1, n2) ->
	if n1 = n2 then true
	else if n2 mod n1 = 0 then false
	else check(n1+1,n2);;	

let rec prime : int -> bool
= fun n -> (* TODO *)
	check(2, n);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->  (* TODO *)
	if a = b then f(a)
	else f a + sigma f (a+1) b;;
