let rec merge : (int list * int list) -> int list = fun (l1,l2) ->
	match (l1,l2) with
	|([], _) -> l2
	|(_, []) -> l1
	|(hd1::tl1, hd2::tl2) -> 
		if(hd1 >= hd2) then hd1::(merge (tl1,l2))
		else hd2::(merge (l1,tl2))

(*
let test = (merge ([55;14;1],[41;33;10;7]))

let rec print_list : int list -> unit = fun l ->
	match l with
	|[] -> ()
	|hd::tl -> print_int hd ; print_string " " ; print_list tl

let a11 = merge ([7; 2; 1], [5; 4; 3]) 
let a12 = merge ([], []) 
let a13 = merge ([9; 2], []) 
let a14 = merge ([], [7; 3]) 
let a15 = merge ([5; 4; 3], [5; 4; 3]) 
let a16 = merge ([5; 3; 1], [8; 6; 4; 2; 0])

let _ = print_list test ; print_list a11; print_list a12 ;  print_list a13;  print_list a14;  print_list a15;  print_list a16; *)


