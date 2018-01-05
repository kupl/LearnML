(*
let emptyQ : queue
let enQ : queue * element -> queue = fun
let deQ : queue -> element * queue =  
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
		type queue = int list list * int list list 
		exception EMPTY_Q
		let emptyQ : queue = ([],[])
		let enQ : queue * element -> queue = fun (q,e) -> 
			match q with
			(llist,rlist) -> (e::llist,rlist)			

		let deQ : queue -> element * queue = fun q -> 
			match q with
			(llist,rlist) -> 
				(match rlist with
				h::t -> (h,(llist,t))
				|[] -> (match llist with
						[] -> raise EMPTY_Q
						|l -> let temp = List.rev l in
							(List.hd temp,(rlist,List.tl temp))
						)
				)
	end

