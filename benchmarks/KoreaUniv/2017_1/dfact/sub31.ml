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