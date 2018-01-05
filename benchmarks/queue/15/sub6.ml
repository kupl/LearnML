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
    type queue = element list
    exception EMPTY_Q
    let emptyQ = []
    let enQ q x = q @ [x]
    let rec deQ q = match q with 
    |[] -> raise EMPTY_Q 
    | h::t -> (h, t)
end
