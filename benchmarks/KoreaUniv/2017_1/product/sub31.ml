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