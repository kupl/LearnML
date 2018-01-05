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
		let enQ = function ((l,r), el) -> (el::l,r)
		let deQ = function ((l,r)) -> (match r with
			[] -> (match List.rev l with
				[] -> raise EMPTY_Q
				| head::tail -> (head,(r,tail))
			)
			| head::tail -> (head,(l,tail))
		)
	end
