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
	type queue = (element list) * (element list)
	
	exception EMPTY_Q

	let emptyQ = ([], [])

	let enQ (queue, element) =
		match queue with
		| (l1, l2) -> (element::l1, l2)
	
	let modifyQ queue =
		match queue with
		| (l1, l2) ->
			if l2 = [] then ([], List.rev l1)
			else queue

	let deQ queue =
		let queue = (modifyQ queue) in
		match queue with
		| (l1, l2) -> 
			if l2 = [] then raise EMPTY_Q
			else ((List.hd l2), (l1, List.tl l2))
end
