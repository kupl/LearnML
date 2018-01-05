module type Queue = sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

module IntListQ = struct
	type element = int list
	type queue = element list * element list 
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ = function (x, y) ->
		match x with (l, r) -> (y::l, r)
	let deQ = 
		let rec rv_concat (l, r) =
			match l with
			| [] -> r
			| hd::tl -> rv_concat (tl, hd::r)
		in
		let flip x = 
			match x with (l, r) -> ([], rv_concat(rv_concat (r, []), rv_concat (l, [])))
		in
		let rec realdeq x =
			match x with
			| ([], []) -> raise EMPTY_Q
			| (l, []) -> realdeq (flip x)
			| (l, hd::tl) -> (hd, (l, tl))
		in
		function x -> realdeq x
			 

end
