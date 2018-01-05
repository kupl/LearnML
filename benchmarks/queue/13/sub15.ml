module type Queue = sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end
module IntListQ(* : Queue with type element = int list*) = struct 
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ : queue * element -> queue = 
		fun (q,e) -> match q with
			(elist1,elist2) -> (e::elist1,elist2)
	let rec deQ : queue -> element * queue =
		let rec pour : queue -> queue =
			fun _q ->
				match _q with
				| ([],_) -> _q
				| (h::t,lst) -> pour (t,h::lst)
		in
		fun _q ->
			match _q with
			| ([],[]) -> raise EMPTY_Q
			| (h::t,[]) -> deQ (pour _q)
			| (lst,h::t) -> (h,(lst,t))
end
