module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> (element * queue)
end

module IntListQ = 
struct
  type element = int list
  type queue = (element list) * (element list)
  exception EMPTY_Q
  let emptyQ = ([], [])
    
  let enQ: queue * element -> queue = function 
    ((left, right), _element) -> (_element :: left, right)
      
  let rec deQ _queue = match _queue with
    |([],[]) -> raise EMPTY_Q
    |(left, head::tail) -> (head, (left, tail))
    |(left,[]) -> deQ(([], List.rev left))
end

