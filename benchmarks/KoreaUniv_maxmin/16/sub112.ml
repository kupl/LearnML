(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
	= fun lst ->
		match lst with
		| [] -> 0
		| hd::[] -> hd
		| hd::tl ->
			let num = max tl in
				if num > hd then num
				else hd;;

(* print_int (max [10; 25; 132]);;
print_endline "";;
print_int (max [1; 4; 132; 551; 32; 15]);;
print_endline "";;
print_int (max []);;
print_endline "";; *)

let rec min : int list -> int
	= fun lst ->
		match lst with
		| [] -> 0
		| hd::[] -> hd
		| hd:: tl ->
			let num = min tl in
				if num < hd then num
				else hd;;
