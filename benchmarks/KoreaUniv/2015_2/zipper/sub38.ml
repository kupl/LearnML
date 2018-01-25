let rec zipper : int list * int list -> int list
=fun (a,b) -> 
match a with
|[] ->
	(match b with
	|[] -> b
	|hd::tl -> hd::zipper([],tl))
|h::t->
	(match b with
	|[] -> h::zipper(t,[])
	|hd::tl->h::hd::zipper(t,tl))
 
