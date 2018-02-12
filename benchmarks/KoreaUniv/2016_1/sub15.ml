(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n=0 then 0 else if n=1 then 1 else fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if (n2 = 0 || n1 = n2) then 1
	else pascal( (n1 - 1) , (n2 - 1) ) + pascal( (n1 - 1) ,n2)		


(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
	if n<=1 then false
	else if n<=3 then true
	else if (n mod 2 = 0 || n mod 3 = 0) then false
	else 
		let rec find i n =
		if i*i<= n then
			if (n mod i = 0) || (n mod (i+2) =0) then false
			else find (i+6) n
		else true
	in find 5 n
		
(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	if a == b then f b
	else f a + sigma f (a+1) b

