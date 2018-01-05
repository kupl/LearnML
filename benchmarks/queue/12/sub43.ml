module type Queue =
  sig
    type element
    
    type queue
    
    exception EMPTY_Q
      
    val emptyQ : queue
      
    val enQ : (queue * element) -> queue
      
    val deQ : queue -> (element * queue)
      
  end
  
module IntListQ =
  struct
    type element = int list
    
    type queue = ((element list) * (element list))
    
    exception EMPTY_Q
      
    let emptyQ : queue = ([], [])
      
    let enQ ((a, b), c) : queue = ((c :: a), b)
      
    let rec deQ (a, b) =
      match b with
      | h :: t -> (h, (a, t))
      | _ ->
          (match a with
           | h :: t -> deQ ([], (List.rev a))
           | _ -> raise EMPTY_Q)
      
  end
  
