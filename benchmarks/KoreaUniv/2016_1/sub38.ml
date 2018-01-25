(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0

let rec fib n = if n = 0 then 0
								else if n = 1 then 1
								else if n < 0 then failwith "error: fib's element (n<0)"
								else fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 
let rec pascal (n1, n2) = if (n1<0||n2<0) then failwith "error : pascal's elements (n1<0 or n2<0)"
											else if (n1<n2) then failwith "error : pascal's elements (n1<n2)"
													else if n2 = 0 then 1
													else if n2 = n1 then 1
													else pascal(n1-1, n2-1) + pascal(n1-1,n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true 

let rec  prime n
= let rec prime_check n d=
	if (n<=0) then failwith "error : prime's element (n<=0)"
	else if (n=2) then true
	else if (n<2) then false
	else if (n=d) then true
	else if ((n mod d)=0) then false
	else prime_check n (d+1)
in if prime_check n 2 then true else false;;


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0

let rec sigma f a b
= if (a<b) then (f b)+(sigma f a (b-1))
else if (a=b) then f a
else (f a) + sigma f (a-1) b;;

