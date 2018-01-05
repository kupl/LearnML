(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 2 - 5 *)
module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ = 
	struct
		type element = int list
		type queue = (int list) list * (int list) list
		exception EMPTY_Q
		let rec putQ : queue -> queue =
			function q ->
			match q with
			| ([], l2) -> q
			| (e::l1, l2) -> putQ(l1, e::l2)
		let emptyQ = ([],[])
		let enQ: queue * element -> queue =
			function ((l1, l2), e) -> (e::l1, l2)
		let deQ: queue -> element * queue =
			function q ->
			match q with
			| (l1, []) -> (
				let temp = putQ(q) in
					match temp with
					| (a, e::b) -> (e, (a, b))
					| (a, []) -> raise EMPTY_Q)
			| (l1, e::l2) -> (e, (l1, l2))
	end