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
    let emptyQ = ([], [])
    let enQ(((a, b)), (x)) = (x :: a, b)                
    let deQ(a, b) = match b with
      | [] -> (match List.rev a with
          | [] -> raise EMPTY_Q
          | x :: y -> (x, ([], y)))
      | x :: y -> (x, (a, y))
  end