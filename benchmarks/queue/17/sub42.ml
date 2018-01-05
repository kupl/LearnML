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
		type queue = (int list) list * (int list) list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ = fun ((a,b), e) -> ((e::a), b) 
		let rec deQ = fun (a, b) ->
			match b with
			|[] -> deQ (b, (List.rev a))
			|_ -> ((List.hd b), (a, List.tl b))
	end
