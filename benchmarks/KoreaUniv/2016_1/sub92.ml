(* Problem 1 *)(*suppose fib 0=0. 0,1,1,2,3... *)
let rec fib : int -> int
= fun n -> if n=0 then 0
					 else if n=1 then 1
					 else fib(n-1)+fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2=0 then 1
									else if n1=n2 then 1
									else pascal(n1-1,n2-1)+pascal(n1-1,n2);;

(* Problem 3 *)
let rec sieve : int * int -> bool (*for divide n *)
= fun (n,d)-> if n=d then true
						  else if(n mod d =0) then false 
							else  sieve (n,d+1);;

let rec prime : int -> bool
= fun n ->if n<2 then false
	        else if (n=2) then true
					else sieve(n,2);; 

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then 0
							 else (f a) + sigma f (a+1) b;;

