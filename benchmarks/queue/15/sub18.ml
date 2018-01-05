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
	type queue = (int list list) * (int list list)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ ((que : queue), (elt : element))= 
		match que with
		(a,b) -> ([elt]@a, b)
    let rec deQ que= 
    	match que with
    	| ([],[]) -> raise EMPTY_Q
    	| (a,[]) -> deQ ([], List.rev a)
    	| (a, hd::tl) -> (hd, (a,tl))
end

