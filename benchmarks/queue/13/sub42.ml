module type Queue =
	sig
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ : Queue =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ = function
				| ((i,o), element) -> (element::i,o)
		let deQ = function
				| ([],[]) -> raise EMPTY_Q
				| (i,[]) -> let ri=List.rev i in
							(List.hd ri, ([],List.tl ri))
				| (i,o) -> (List.hd o, (i,List.tl o))
	end