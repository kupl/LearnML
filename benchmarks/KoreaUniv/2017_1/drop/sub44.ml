(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
			| [] -> []
			| hd::tl -> match n with
						|0 -> hd::tl
						|_ -> drop tl (n-1);;
