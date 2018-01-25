(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n=0 then 0
else if n=1 then 1
else fib (n-1) + fib (n-2);;
 
(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1=0 then 1
else if n1=1 then 1
else if n2=0 then 1
else if n1=n2 then 1
else pascal (n1-1,n2-1) + pascal (n1-1,n2);;


(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let i = n-1 in 
let rec rprime a = 
if a = 1 then true 
else if n mod a = 0 then false 
else rprime(a-1) in rprime i;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> let rec rsigma a b =
if a > b then 0 else f a + rsigma (a+1) b in
rsigma a b;;


