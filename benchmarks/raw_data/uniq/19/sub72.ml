let uniq : 'a list -> 'a list
= fun lst -> 
	let rec urc n l=
		match l with
		| [] -> []
		| hd::tl -> if hd = n then urc n tl else hd::urc n tl
	in let rec url l=
		match l with
		| [] -> []
		| hd::tl -> hd::url (urc hd tl)
	in url lst;;