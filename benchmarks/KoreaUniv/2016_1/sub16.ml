(* Poroblem 1*)
let rec fib n =
	if n <= 2 then 1
	else fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal (n1, n2) =
	match (n1, n2) with
	(n1, 0) -> 1
	|n1 = n2 -> 1
	|_ -> pascal (n1-1, n2-1) + pascal (n1-1, n2);;

(* Problem 3 *)
let rec prime n =
	let rec nd k =
	n mod k <> 0 && nd (k+1) in
	n <> 1 && nd 2;;


(* Problem 4 *)
let rec sigma f a b =
	let rec f = (fun n -> n)  in
	if
	a = b then f a
	else f a  + sigma f a (b-1);;
