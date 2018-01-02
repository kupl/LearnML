let rec help max base lst =
	match lst with
	|[] -> max
	|hd::tl -> 
		let base = base+hd in
		if base > max then max else help max base tl ;;

let rec f lst = 
	match lst with
	|[] -> 0
	|hd::tl -> help (f tl) 0 lst ;;


(*
let rec help : int -> int -> int list -> int
= fun max base lst ->
	match lst with
	|[] -> max
	|hd::tl -> 
		let base = base+hd in
		if base > max then help hd base tl else help max base tl 

let rec sub_max : int list -> int
= fun lst ->
	match lst with
	|[] -> 0
	|hd::tl -> help (sub_max tl) 0 lst
*)
