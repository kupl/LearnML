exception E;;
let rec zipper : int list * int list -> int list = fun (a,b)->
match a with
| [] -> b
| hd::tl -> match b with
	| [] -> hd::(zipper (tl,[]))
	| h::t -> hd::h::(zipper (tl,t));;
