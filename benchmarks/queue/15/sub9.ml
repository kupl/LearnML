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
	type queue = Empty | Queue of element list * element list
	exception EMPTY_Q
	let emptyQ = Empty
	let enQ(q,e) =
		match q with
		Empty -> Queue(e::[], [])
		| Queue(l1, l2) -> Queue(e::l1, l2)
	let rec deQ(q) =
		match q with
		Empty -> raise (EMPTY_Q)
		| Queue(l1,[]) -> 
			if(List.length l1 == 1) then (List.hd l1, emptyQ)
			else deQ(Queue([], List.rev l1))
		| Queue(l1,e::l2) -> (e, Queue(l1, l2))
end


module ValidIntListQ = (IntListQ : Queue)