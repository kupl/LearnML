module type Queue = 
	sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end

module IntListQ  = 
	struct
	type element = int list
	type queue = int list list * int list list
	exception EMPTY_Q
	
	let emptyQ = ([],[])

	let enQ (que,x) = 
		match que with
		|(a,b) -> (x::a,b)
	let rec deQ que = 
		match que with
		|([],[]) -> raise EMPTY_Q
		|(a,[]) -> (deQ ([],(List.rev(a))))
		|(a,b) -> ((List.hd(b)),(a,(List.tl(b))))
end
