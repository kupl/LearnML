(*
 * Programming Languages, 2013 Fall.
 * HW Code for Exercise 2-3
 * Department of Computer Science and Engineering
 * 2006-11855, Jung Yonghyuk (ever103@snu.ac.kr)
 *)

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
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = (([], []) : queue)
		let enQ =
			fun ((q : queue), (e : element)) -> 
				match q with
				| (l, r) -> ((e::l, r) : queue)
		let rec deQ (q : queue) =
			match q with
			| ([], []) -> raise EMPTY_Q
			| (l, rh::rt) -> (rh, ((l, rt) : queue))
			| (l, []) -> (deQ ([], (List.rev l)))
	end
