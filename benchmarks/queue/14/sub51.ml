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
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ = function ((a, b), e) -> (e::a, b)
		let rec deQ =
		function ([], []) -> raise EMPTY_Q
		       | (a, b) ->
			   match b with
			     [] -> (deQ ([], List.rev a))
			   | l::r -> (l, (a, r))
	end
