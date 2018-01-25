let rec fib : int -> int = fun n -> 

	match n with 
	|1 -> 1
	|2 -> 1
	|_ -> fib(n-1)+fib(n-2);;



let rec pascal : int * int -> int
= fun (n1, n2) -> 
	match (n1, n2) with 
	|(n1,0) -> 1
	|_ -> if n1=n2 then 1 else pascal ( n1-1, n2) + pascal (n1-1, n2-1);;


let rec prime : int -> bool
= fun n -> 
	if n<=1 then false
	else

	let rec divide a =
		if a=1 then true 	 
		else  if n mod a=0 then false 
		else divide (a-1)   
	in divide (n-1);;

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if a<b then ( f a ) + ( sigma f (a+1) b ) 
else (f b);;

