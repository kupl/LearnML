
(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
	if n < 0 then raise (Failure "cannot drop minus times") else
	match n with
	|0 -> l
	|_ -> match l with
		  |[] -> []
		  |hd::tl -> drop tl (n-1)