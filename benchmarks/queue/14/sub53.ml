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
	type queue = EMPTY | QUEUE of element list * element list
	exception EMPTY_Q
	let emptyQ = EMPTY
	let enQ = function (q,elt) -> 
		(match q with
		| QUEUE(l1,l2) -> QUEUE(elt::l1,l2)
		| EMPTY -> QUEUE([elt], []))
	let deQ = function q ->
		(match q with
		| QUEUE(l,[]) ->	let revList = List.rev l in
							(List.hd revList, QUEUE([], List.tl revList))
		| QUEUE(l,head::tail) -> (head, QUEUE(l,tail))
		| EMPTY -> raise EMPTY_Q)
end