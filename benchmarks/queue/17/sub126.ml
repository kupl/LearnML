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
	type queue = QUE of element list * element list
	exception EMPTY_Q
	let emptyQ = QUE([],[])
	let enQ (q, e) = match q with
		QUE (x, y) -> QUE(e::x, y)
	let rec deQ q = (match q with
		  QUE ([], []) -> raise (EMPTY_Q)
		| QUE (x, []) -> deQ( QUE([], List.rev(x)))
		| QUE (x, r::y) -> (r, QUE(x, y)) )
end


