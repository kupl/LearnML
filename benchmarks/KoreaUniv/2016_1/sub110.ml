(* Problem 1 *)
let rec fib : int -> int
= fun n ->
	if n = 0 then 0
	else if n = 1 then 1
	else fib(n-2) + fib(n-1);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n2 <0 || n1<0 then 0
	else if n2>n1 then 0
	else if n2 = 0 then 1
	else if n1=n2 then 1
	else pascal(n1-1,n2-1) + pascal(n1-1,n2);;


(* Problem 3 *)
let prime : int -> bool
= fun n ->
	let n = abs n in
    let rec div d =
    	d * d > n || ( div (d+1) && n mod d <> 0 ) 
  		in n <> 1 && div 2;;


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	let n = abs b in
		if a <= n then sigma f a (n-1) + f n
		else 0;;
		