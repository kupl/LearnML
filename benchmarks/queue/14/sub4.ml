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
    let enQ = (fun ((l,r),e) -> (e::l,r))
    let deQ = (fun (l,r) -> if (List.length r) = 0
                             then if (List.length l) = 0
                                   then raise EMPTY_Q                                  
                                   else ((List.hd (List.rev l)),(r,(List.tl
                                   (List.rev l))))   
                             else ((List.hd r),(l,(List.tl r))))       
  end
