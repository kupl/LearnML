exception Empty_Queue of string
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
		let emptyQ : queue = ([], []) 
		let enQ = function ((q:queue), (e:element)) ->
			match q with
				(q1, q2) -> ((e::q1, q2):queue)
		let deQ = function (q:queue) ->
			match q with
				| ([], []) -> raise (Empty_Queue "Queue is empty")
				| (q1, []) -> (((List.hd (List.rev q1)):element), (([], List.tl (List.rev q1)):queue))
				| (q1, h::t) -> (h, (q1, t))
	end
