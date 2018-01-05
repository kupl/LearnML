(* 2008-11874 Lee, Sujee *)
(* EXERCISE 5 *)	
module type Queue = 
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end 

module IntListQ : Queue with type element = int list = 
struct 
  type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ ((lq,rq),ele) = ((ele::lq),rq)
	let deQ (lq,rq) = 
		if (rq<>[]) then ((List.hd rq),(lq,(List.tl rq)))
		else if (rq=[])&&(lq<>[]) then ((List.hd (List.rev lq)),([],(List.tl (List.rev lq))))
		else raise EMPTY_Q
end 