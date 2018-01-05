module type Queue =
    sig 
      type element
      type queue
      exception EMPTY_Q

      val emptyQ : queue
      val enQ : queue * element -> queue
      val deQ : queue -> element * queue
    end

module IntListQ  = 
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q

    let emptyQ : queue= ([],[])
    let enQ (q, e)  = 
      match q with
      | (lq, rq) -> (e::lq, rq)

    let deQ q =
      match q with
      | ([], []) -> raise EMPTY_Q
      | (lq, []) -> 
	  let rq = List.rev lq in
	  (List.hd rq, ([], List.tl rq))
      | (lq, rq) -> (List.hd rq, (lq, List.tl rq))
      
  end
