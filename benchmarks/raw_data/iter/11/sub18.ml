(* 2008-11874 Lee, Sujee *)
(* EXERCISE 2 *)

let rec iter(n,f) x = (* iter : int * ('a -> 'a) -> 'a -> 'a = <fun> *)
	let identity x = x
	in
	if n=0 then identity x
	else if n=1 then f x
	else iter(n-1, f) (f x)
	
	
	(*
let sum2 = iter(10, fun x -> 2+x) 0

let _ = 
	print_string "EXERCISE 2 : ";
	print_int sum2;
	print_newline()
	*)