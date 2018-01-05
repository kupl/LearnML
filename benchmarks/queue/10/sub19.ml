(* Exercise 4 *)
module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end

module IntListQ =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ : queue = ([], [])
		let enQ ((que : queue), (ele : element)) : queue =
			match que with
				(l, r) ->
					((ele :: l), r)
		let deQ (que : queue) : (element * queue) =
			let que =
				match que with
					(l, []) ->
						([], (List.rev l))
					| _ ->
						que
			in
			match que with
				([], []) ->
					raise EMPTY_Q
				| (l, r) ->
					((List.hd r), (l, (List.tl r)))
	end
