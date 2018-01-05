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
    let emptyQ = ([],[])
    let enQ: queue * element -> queue = fun (q,e) -> match q with
      (l,r) -> (e::l, r)
    let deQ: queue -> element * queue = fun q -> match q with
      ([],[]) -> raise EMPTY_Q
      | (left,[]) -> let revL = (List.rev left) in 
          (List.hd revL, ([], List.tl revL))
      | (left,r::right) -> (r, (left, right))
  end

module ValideIntListQ = (IntListQ : Queue)

