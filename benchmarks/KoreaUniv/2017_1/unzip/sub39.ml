(*problem 7*)
let rec split_first : ('a * 'b)list -> 'a list
= fun lst ->
	match lst with
		| [] -> []
		| (a, b)::tl -> 
			if(tl = []) then a::[]
			else a::(split_first tl)

let rec split_second : ('a * 'b)list -> 'b list
= fun lst ->
	match lst with
		| [] -> []
		| (a, b)::tl ->
			if(tl = []) then b::[]
			else b::(split_second tl)


let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
	if(lst = []) then ([], [])
	else (split_first lst, split_second lst)
