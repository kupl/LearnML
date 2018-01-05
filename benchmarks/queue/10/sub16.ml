

(*Ex4*)

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
	type queue = (int list) list * (int list) list 
	exception EMPTY_Q
	let (emptyQ : queue) = ([],[])
	let (enQ:queue*element -> queue) = fun(q,e)-> match q with (a,b) -> (e::a,b)
	let rec (deQ:queue -> element * queue) = fun q ->
				match q with ([],[]) -> raise EMPTY_Q
						|	(a,[]) -> deQ ([],(List.rev a))
						|	(a,h::t) -> (h, (a,t))
end
