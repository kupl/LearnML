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
    type queue = QUEUE of int list * int list 
    exception EMPTY_Q
    let emptyQ = QUEUE([],[])
    let enQ (q,e) =
     match q with
    |QUEUE(l,r) -> QUEUE(List.append (List.rev e) l,r) 
    let deQ q =
      match q with
      |QUEUE([],[]) -> raise EMPTY_Q
      |QUEUE(l,[]) -> ([List.hd (List.rev l)], QUEUE([],List.tl (List.rev l)))
      |QUEUE(l,pop::r) -> ([pop], QUEUE(l,r))
  end
