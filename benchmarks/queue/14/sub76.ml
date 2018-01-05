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
	type queue = element list* element list
	
	exception EMPTY_Q

	let emptyQ : queue = ([], [])

	let enQ : queue * element -> queue = fun (queue, x) ->
		match queue with
			| (front, back) -> (x::front, back)

	let rec deQ : queue -> element * queue = fun queue ->
		match queue with
			| ([], []) -> raise EMPTY_Q
			| (front, []) -> deQ ([], List.rev front)							
			| (front, x :: back) -> (x, (front, back))
end