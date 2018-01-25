(*********************) (* Problem 1: filter *) (*********************) 
let rec filter pred lst = [] let rec filter f a = 
	match a with
	[] -> raise(Failure"List is empty")
	| hd::tl -> if ( f hd ) then hd::(filter f tl) else
	filter f tl
