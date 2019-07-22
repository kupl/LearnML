(* 2008-11874 Lee, Sujee *)
(* EXERCISE 1 *)

let rec sigma(a,b,f) = (* sigma : int * int * (int -> int) -> int = <fun> *)
	if b>a then (f b) + (sigma(a,b-1,f))
	else if b=a then (f b)
	else 0
		
(*	
let sum = sigma(1,5,fun n -> n+1)

let _ = 
	print_string "EXERCISE 1 : ";
	print_int sum;
	print_newline()
*)