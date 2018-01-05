module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
end

module IntListQ =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([],[])  
		let enQ = function
			| (q, e) ->
				begin match q with
				| (left, right) -> (e::left, right)
				end;;
		let deQ = function
			| ([], []) -> raise EMPTY_Q
			| (left,[]) ->
				begin match (List.rev left) with
				| e::lst -> (e,([],lst)) end 
			| (left, e::right) -> (e,(left,right));; 
end 