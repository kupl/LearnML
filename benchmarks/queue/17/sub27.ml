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
    type queue = int list list list
    exception EMPTY_Q
    let emptyQ = [[]; []]
    let enQ (q, elmt) =  [[elmt]@(List.hd q); (List.hd (List.tl q))]
    let rec deQ q =
      if q = emptyQ then raise EMPTY_Q
                    else match q with
                    [a; b] -> if b = [] then deQ [[]; (List.rev a)]
                                        else ((List.hd b), [a; (List.tl b)])
  end


