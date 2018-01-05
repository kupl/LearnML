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
	type queue = (element list)*(element list)
	exception EMPTY_Q
	let emptyQ =([],[])
	let enQ : queue * element -> queue = fun (q,el) -> match q with (lst1,lst2) -> (el::lst1,lst2)
	let deQ : queue -> element * queue= fun q ->match q with (a,h::t)-> (h,(a,t))
					|(a,[])-> (match (List.rev a) with h::t-> (h,([],t))
									|[]-> raise EMPTY_Q)
end
