module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

let rec invert (original, result) =
	match original with
	[] -> result
	| leftmost::others -> invert(others, leftmost::result)

module IntListQ =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ (q, e) =
			match q with
			(ls, rs) -> (e::ls, rs)
		let rec deQ q =
			match q with
			([], []) -> raise EMPTY_Q
			| (ls, []) -> deQ ([], invert (ls, []))
			| (ls, e::rs) -> (e, (ls, rs))
	end
