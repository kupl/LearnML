let rec zipper a b = 
	match a with
	[] -> b
	| hd::tl -> match b with
	[] -> a
	| h::t -> hd::h::(zipper tl t)
