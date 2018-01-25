(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	match lst with
	[] -> ([], [])
	| hd::tl -> 
		let (a, b) = hd in
		let unzip_tail = unzip tl in
		let (alist, blist) = unzip_tail in
		(a::alist, b::blist)
;;