(*problem 5*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if(a==b) then a
	else b * (product f a (b-1))

let rec fastexpt = fun b n ->
	if (n == 0) then 1
	else if (n mod 2 == 0) then ((fastexpt b (n/2)) * (fastexpt b (n/2)))
	else b * (fastexpt b (n-1))

let rec dfact : int -> int
	= fun n ->
	if((n mod 2) = 0) then (product (fun x -> x) 2 (n/2)) * fastexpt 2 (n/2)
	else (product (fun x -> x) 1 n)/(dfact (n-1))