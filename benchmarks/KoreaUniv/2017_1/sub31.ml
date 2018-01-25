(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n -> 
	if n < 0 then raise (Failure "minus n") else
	match n with
	|0 -> 1
	|1 -> b
	|_ -> if (n mod 2 = 0) then (fun a -> a*a) (fastexpt b (n/2))
		  else b * (fastexpt b (n-1))

(* problem 2*)

let rec divisor : int -> int -> int
= fun n i ->
	if n <= 0 then raise (Failure "wrong input n") else
	if i*i > n then n 
	else if (n mod i = 0) then i
	else (divisor n (i+1))

let smallest_divisor : int -> int
= fun n -> divisor n 2 

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
	if n < 0 then raise (Failure "cannot iterate minus times") else
	match n with
	|0 -> (fun x -> x)
	|_ -> (fun x -> f((iter (n-1, f)) x))

(* problem 4*)

let rec make_list a b =
	if a > b then raise (Failure "wrong low and high") 
	else if a = b then a::[]
	else a::(make_list (a+1) b)

let rec product_list f lst =
	match lst with
	|[] -> 1
	|hd::tl -> (f hd)*(product_list f tl)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
	let list_todo = make_list a b in
		product_list f list_todo

(* problem 5*)

let dfact : int -> int
= fun n ->
	if n < 0 then raise (Failure "minus input") else
	if n = 0 then 1 else
	if (n mod 2 = 0) then product (fun x -> 2*x) 1 (n/2)
	else product (fun x -> 2*x-1) 1 ((n+1)/2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
	if n < 0 then raise (Failure "cannot drop minus times") else
	match n with
	|0 -> l
	|_ -> match l with
		  |[] -> []
		  |hd::tl -> drop tl (n-1)

(* problem 7*)

let join_tuple tuple1 tuple2 = 
	let (elt1, elt2) = tuple1 in
		let (elt3, elt4) = tuple2 in
			(elt1::elt3, elt2::elt4)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	match lst with
	|[] -> ([],[])
	|hd::tl -> join_tuple hd (unzip tl)

(* problem 8*)

let rec length l =
	match l with
	|[] -> 0
	|hd::tl -> 1+(length tl)

let rec nth l n =
	match l with
	|[] -> raise (Failure "list is too short")
	|hd::tl -> if n = 0 then hd
			   else nth tl (n-1)

let rec reverse l =
	match l with
	|[] -> []
	|hd::tl -> (reverse tl)@[hd]

let rec insert a l =
	match l with
	|[] -> [a]
	|hd::tl -> if a < hd then a::hd::tl
			   else hd::(insert a tl)

let rec list_sort lst =
	match lst with
	|[] -> []
	|hd::tl -> insert hd (list_sort tl)

let descending_sort lst = reverse(list_sort(lst))

let rec count_options (coins, amount, index) =
	if amount = 0 then 1
	else if (amount < 0)||(index >= length(coins)) then 0
	else count_options(coins, amount, (index+1)) + count_options(coins, (amount - nth coins index), index)  

let change : int list -> int -> int
= fun coins amount -> 
	count_options (descending_sort coins, amount, 0)



