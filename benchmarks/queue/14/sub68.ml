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
	type queue = element list list
	exception EMPTY_Q
	let emptyQ:  queue
		 = [[];[]]
	let enQ : queue*element ->
			queue = fun (q, e)->
		match q with
		|[r;l] -> [e::r;l]
	let deQ : queue -> element*queue = fun q ->
		match q with
		|[r;[]] -> (List.hd (List.rev r) ,[[]; List.tl (List.rev r)])
		|[r;hd::tl] -> (hd, [r;tl])
		|[[];[]] -> raise EMPTY_Q
end