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
	  type queue = element list * element list
	  exception EMPTY_Q
	  let emptyQ = ([],[]) 
	  let enQ(q,(elem:int list)) =
		match q with
		  (i,o) -> ( ((List.append [elem] i), o) )
	  let deQ(q) = 
		match q with
		  ([],[]) -> raise EMPTY_Q
		| (i,[]) -> (List.hd(List.rev(i)),([],(List.tl(List.rev(i)))))
		| (i,o) -> (List.hd(o), (i,(List.tl(o))))
	end


