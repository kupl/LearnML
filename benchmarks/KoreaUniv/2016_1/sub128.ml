(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 then 1 else fib(n-1) + fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 then 1 else if n2 = n1 then 1 else pascal(n1-1,n2-1) + pascal (n1-1,n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> if n <= 1 then false 
           else if n <= 3 then true 
           else if n mod 2 = 0 then false 
           else if n mod 3 = 0 then false
	   else let rec other_test h = if h * h >= n then true 
					else if n mod h = 0 then false
					else other_test(h+2)
	   in other_test(5);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->0 (* if a > b then 0 else f(a) + sigma(f,a+1,b) Could not get ocaml to accept this without 'a->'b->int not being in error*);;					
