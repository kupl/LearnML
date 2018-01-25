let rec fastexpt : int -> int -> int = fun b n ->
	if n<0 then raise (Failure "n is negative number.")
	else if n=0 then 1
	else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
	else b*(fastexpt b (n-1));;

let rec smallest_divisor : int -> int = fun n ->
	if n<0 then raise (Failure "n is negative number.")
	else if n mod 2 = 0 then 2
  else let x = n in
       let rec f x i = 
         if i > int_of_float (sqrt (float_of_int x)) then x
         else if x mod i = 0 then i
         else f x (i+2) in f x 3;;

let rec iter : int * (int -> int) -> (int -> int) = fun (n,f) ->
	if n<0 then raise (Failure "n is negative number.")	
	else if n = 0 then fun x -> x
	else fun x -> (iter (n-1,f)) (f x);;

let rec product : (int -> int) -> int -> int -> int = fun f a b ->
	if a>b then raise (Failure "a is larger then b.")
	else if a = b then f b
	else (f a) * (product f (a+1) b);;

let rec dfact : int -> int = fun n ->
	match n with
	1 -> 1
 |2 -> 2
 |_ -> n*(dfact (n-2));;

let rec drop : 'a list -> int -> 'a list = fun l n ->
	if n<0 then raise (Failure "n is negative number.")	
	else if n=0 then l else (match l with [] -> [] |hd::tl -> drop tl (n-1));;

let rec unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
	match lst with
	[] -> ([],[])
	|(a,b)::tl -> let (l1,l2) = unzip tl in (a::l1,b::l2);;

let rec change : int list -> int -> int = fun coins amount ->
	if amount=0 then 1
	else if amount<0 then 0
	else
		match coins with
		[] -> 0
		|hd::tl -> (change coins (amount-hd) + change tl amount);;
