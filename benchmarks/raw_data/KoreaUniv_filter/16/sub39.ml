let rec filter f l =
	match l with
	|[]->[]
	|hd::tl -> if f hd then [hd] @ (filter f tl)
						else (filter f tl)
