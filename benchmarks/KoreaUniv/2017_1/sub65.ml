(* problem 1*)
let rec fastexpt : int -> int -> int = fun b n 
-> if n = 0 then 1 
	else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2)) 
		else b*fastexpt b (n-1);;

(*problem 2*)
let smallest_divisor : int -> int = fun n 
-> let rec sd n i 
= if i*i>n then n 
	else if (n mod i = 0) then i 
		else sd n (i + 1) in sd n 2;;

(*problem 3*)
let rec iter : int * (int -> int) -> (int -> int) = fun (n,f) 
-> if n = 0 then iter(1,fun x->x) 
	else if n = 1 then f 
		else fun x -> f (iter(n-1,f) x) ;;

(*problem 4*)
let rec product : (int -> int) -> int -> int -> int = fun f a b 
-> if b = a then a 
	else b * product f a (b-1);;

(*problem 5*)
let rec dfact : int -> int = fun n 
-> if n = 1 then 1 
	else if n = 2 then 2 
		else n*(dfact(n-2));;

(*problem 6*)
let rec drop : 'a list -> int -> 'a list = fun l n 
-> if n >= List.length l then [] 
	else if n=1 then List.tl l 
		else drop (List.tl l) (n-1);;

(*problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list = fun lst 
-> List.split lst;;

(*problem 8*)
let rec change : int list-> int -> int = fun coins amount 
-> let rec total amount coins i
= if amount < 0 then 0
else if amount = 0 then 1
else if i = List.length coins then 0
else ((total (amount - (List.nth coins i)) coins i) + (total amount coins (i+1)))
in total amount coins 0;;
