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
	  let emptyQ = ([], [])
	  let enQ(q, e) =
	  		match q with
			| (l,r) -> (e::l, r)

	  let deQ(q) =
	  		match q with
			| ([], []) -> raise (EMPTY_Q)
			| (l, []) -> (List.hd((List.rev l)), ([], List.tl(List.rev l)) )
			| ([], r) -> (List.hd r, ([], List.tl r))
			| (l, r) -> (List.hd r, (l, List.tl r) )

    end 

