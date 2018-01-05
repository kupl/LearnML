module type Queue = 
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end

module IntListQ =
struct
	type element = int list
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (a,b) = 
		match a with
		|(s,t) -> (b::s,t)
	let deQ b = 
		match b with
		|(a,hd::tl)-> (hd,(a,tl))
		|(a,[]) -> let s = ([],(List.rev a)) in
				   (match s with
				   |(q,hd::tl) -> (hd,(q,tl))
				   |(_,[]) -> raise EMPTY_Q)
end
