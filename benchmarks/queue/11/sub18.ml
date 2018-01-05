

module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

module IntListQ : Queue with type element = int list =
struct 
	type element = int list
	type queue = ( (element list)*(element list))
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (qu, el)= 
   		(match qu with
		( lst, lst2 ) -> ( (el::lst), lst2 ))
	let deQ qu= 
		match qu with
		( [], []) -> raise EMPTY_Q
		| ( lst, lst2 ) -> 
			(match lst2 with
			 [] -> ( (List.hd (List.rev lst)), ([], (List.tl (List.rev lst))))
			| h::t -> ( h , ( lst, t)))
end

