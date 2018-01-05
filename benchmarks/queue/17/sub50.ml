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
		let enQ ((q: queue), (e: element)) : queue =
			match q with 
			| (l, r) -> (e::l, r)

		let deQ (q: queue): element * queue = 
			match q with
			| ([], []) -> raise EMPTY_Q
			| (l, []) -> (List.hd(List.rev(l)), ([], List.tl(List.rev(l))))
			| (l, hd::tl) -> (hd, (l, tl))



end