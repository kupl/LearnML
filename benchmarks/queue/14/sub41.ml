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
	let enQ : queue * element -> queue =
		fun a -> 
			match a with
				|(c, d) -> 
							match c with 
								|(e, f) -> ((List.append [d] e), (List.rev (List.append [d] e)) ) 
	let deQ : queue -> element * queue = 
		fun a -> 
			match a with
				|(c, d) -> if (List.length c) == 0 then raise EMPTY_Q 
							else ((List.hd d), ((List.rev (List.tl d)), (List.tl d)))
						
end


