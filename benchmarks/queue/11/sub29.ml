(* 컴퓨터공학부 2009-11833 창배성*)

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
		type queue
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ a =
			match a with
			(q, e) -> match q with
				(a, b) -> (e::a, b)
		let deQ x =
			match x with
			([], []) -> raise EMPTY_Q
			| (y, []) -> ((List.hd (List.rev y)) ,([], (List.tl (List.rev y))))
			| (y, z) -> ((List.hd z), (y, (List.tl z)))
	end