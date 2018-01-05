
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
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ = function ((ll, rl), e) -> (e::ll, rl)
		let deQ = function q -> match q with
					| ([], []) -> raise EMPTY_Q
					| (ll, []) -> let reverse = List.rev ll in (List.hd reverse, ([], List.tl reverse))
					| (ll, h::t) -> (h, (ll, t))
	end
