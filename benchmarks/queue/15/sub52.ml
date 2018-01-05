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

	let emptyQ : queue = ([], [])

	let enQ : queue * element -> queue = 
		fun (queue, element) ->
			match queue with
			| ([], []) -> ([], [element])
			| (front, back) -> (element::front, back)

	let rec deQ : queue -> element * queue = 
		fun queue ->
			match queue with
			| ([], []) -> raise EMPTY_Q
			| (front, []) -> deQ ([], (List.rev front))							
			| (front, hd::tail) -> (hd, (front, tail))
end