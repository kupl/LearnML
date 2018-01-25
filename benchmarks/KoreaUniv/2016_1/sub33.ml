(* Problem 1 *)
let rec fib : int -> int
= fun n ->
	match n with
	0 -> 0
	|1 -> 1 
	|_ ->  fib (n-1)+ fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
 (* TODO *)
	match n1 with 
	0 -> 1
	|1 -> 1	  
	|_ ->if n2=0||n2=n1  then 1	else pascal(n1-1,n2-1) + pascal(n1-1,n2)


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

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> (* TODO *)
if b>a then sigma f (a+1) b + f a
else f a
