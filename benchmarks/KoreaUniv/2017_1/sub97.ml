

(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n=0 then 1
	else if (n mod 2 =1) then b * (fastexpt b (n-1))
	else (fastexpt b (n/2)) * (fastexpt b (n/2));;

(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
	if n=1 then 1
	else if (n mod 2 = 0) then 2
	else let rec test x y = if(y*y > x) then x else if (x mod y =0) then y else (test x (y+2)) in
	test n 3;;

(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
	if n=0 then (fun x -> x)
    else if n=1 then (fun x -> f x)
	else (fun x -> f (iter (n-1,f) x) );;

(* problem 4*)
let rec product : (int-> int) -> int -> int -> int
= fun f a b ->
	if (a=b) then (f a)
	else (f a) * (product f (a+1) b);;

(* problem 5*)
let dfact : int -> int
= fun n -> 
	if (n mod 2 = 0) then product (fun x -> 2*x) 1 (n/2)
	else product (fun x -> 2*x-1) 1 ((n+1)/2);;

(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n ->
	if n=0 then l
	else match l with
	| [] -> []
	| hd::tl -> drop tl (n-1);;

(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	let rec test f l = 
		match l with
		|[] -> []
		|hd::tl -> f hd :: test f tl in 
	let l1 = test (fun (x,_) -> x) lst in
	let l2 = test (fun (_,x) -> x) lst in
	(l1,l2);;

(* problem 8*)
let change : int list -> int -> int
= fun coins amount ->
	if amount<0 then 0
	else match coins with
		|[]-> 0
		|hd::tl -> (*let coins = sort coins in *)
		
	let result = Array.make (amount + 1) 0 in
	Array.set result 0 1;

	let changes a coin = 
		for i = 1 to ((Array.length a)-1) do
			if ((i-coin)>=0) then Array.set a i ((Array.get a i) +(Array.get a (i-coin)))
		done in

	for i = 0 to ((List.length coins)-1) do
		changes result (List.nth coins i)
	done;
	Array.get result amount;;

(*#use "test3.ml";;*)






	  



