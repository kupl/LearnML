
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec badd : bin -> bin -> bin
= fun b1 b2 ->
	match reverse_list b1 with
	|[] -> b2
	|hd1::tl1 -> 
		match reverse_list b2 with
		|[] -> b1
		|hd2::tl2 -> match hd1 with
			|ZERO -> if hd2 = ZERO then (badd (reverse_list tl1) (reverse_list tl2))@[ZERO]
					 else (badd (reverse_list tl1) (reverse_list tl2))@[ONE] 
			|ONE -> if hd2 = ZERO then (badd (reverse_list tl1) (reverse_list tl2))@[ONE]
					else (badd [ONE] (badd (reverse_list tl1) (reverse_list tl2)))@[ZERO]

let rec remove_frontzero
= fun b ->
	match b with
	|[] -> []
	|hd::tl -> match tl with
			   |[] -> hd::[]
			   |_ -> match hd with
			   		 |ZERO -> tl
			   		 |ONE -> b

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> 
	match reverse_list b2 with
	|[] -> []
	|hd::tl ->
		match hd with
		|ZERO -> remove_frontzero ((bmul b1 (reverse_list tl))@[ZERO]) 
		|ONE -> remove_frontzero (badd ((bmul b1 (reverse_list tl))@[ZERO]) b1)

