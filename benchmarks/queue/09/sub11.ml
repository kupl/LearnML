(* 2006-11782 Song Young-chan, Hw2-6 Queue=2Stack *)

module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end

module IntListQ : Queue with type element = int list = 
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = (([]:element list), ([]:element list))
		let enQ = fun (((left_queue:element list), (right_queue:element list)), (input:element)) -> (input::left_queue, right_queue)
		let deQ = fun (left_queue, right_queue) ->
				if ((left_queue, right_queue) = emptyQ) then raise (EMPTY_Q)
				else if (right_queue = []) then ((List.hd (List.rev left_queue)), ([], (List.tl (List.rev left_queue))))
				else ((List.hd right_queue), (left_queue, (List.tl right_queue)))
	end
