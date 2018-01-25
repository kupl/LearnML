(* Problem 1 *)
(* Fibonacci Sequence *)
let rec fib : int -> int
= fun n -> 
	match n with
	| 0 -> 0
	| 1 -> 1
	| _ -> fib (n - 1) + fib (n - 2);;
	
(* let () =
	for n = 1 to 16 do
		Printf.printf "%d, " (fib n)
	done;
	print_endline "..." *)

(* Problem 2 *)
(* Pascal's Triangle *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
	if n1 < 0 || n2 < 0 then 0
	else if n2 > n1 then 0
	else if n1 = 0 then 1
	else if n2 = n1 then 1
else (pascal ((n1 - 1), (n2 - 1))) + (pascal ((n1 - 1), n2));;

(* print_int (pascal (-1,-1));; print_endline "";; *)
(* print_int (pascal (-1,0));; print_endline "";; *)
(* print_int (pascal (0,1));; print_endline "";; *)
(* print_int (pascal (1,3));; print_endline "";; *)
(* print_int (pascal (2,-2));; print_endline "";; *)
(* print_int (pascal (0,0));; print_endline "";; *)
(* print_int (pascal (1,0));; print_endline "";; *)
(* print_int (pascal (1,1));; print_endline "";; *)
(* print_int (pascal (2,1));; print_endline "";; *)
(* print_int (pascal (4,2));; print_endline "";; *)

(* Problem 3 *)
(* Prime Number *)
let rec prime_inner : int -> int -> bool
= fun n d ->
	if n < 1 then false
	else if n <= d then true
	else if n mod d = 0 then false
else (prime_inner n (d+1));;

let rec prime : int -> bool
= fun n -> 
	prime_inner n 2;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a > b then 0
	else (f a) + (sigma f (a+1) b);;

(* print_int sigma (fun x -> x) 1 10;; *)
(* print_int sigma (fun x -> x*x) 1 7;; *)

