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
	let emptyQ = ([],[])
	let enQ (q,elem) = 
		match q with
		(first,second) -> (elem::first, second)
	let deQ q = 
		match q with
		([],[]) -> raise EMPTY_Q
		|(s,k) -> (match k with
				[] -> let p = List.rev s in
					(match p with
					[]-> raise EMPTY_Q
					|[h] -> (h, ([],[]))
					|h::t -> (h, ([],t)))
				|h::t -> (h, (s,t)))
	end
