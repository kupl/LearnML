


module type Queue = sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue 
	val deQ: queue -> element * queue
end

module IntListQ = struct
	type element = int list 
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ(que, ele) = 
		match que with
		| (first, second) -> (ele::first, second)
	let deQ(que) = 
		match que with
		| ([], []) -> raise EMPTY_Q
		| (first, []) -> (List.hd(List.rev(first)), ([], List.tl(List.rev(first))))
		| (first, second) -> (List.hd(second), (first, List.tl(second)))
end


module ValidIntListQ = (IntListQ: Queue)
