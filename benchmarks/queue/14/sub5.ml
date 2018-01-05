module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ : queue
    val enQ : queue * element -> queue
    val deQ : queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list 
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ = 
      fun (q, e) -> 
        (e :: (fst q), (snd q))
    let rec deQ = 
      fun q ->
        match q with
        | [], [] -> raise EMPTY_Q
        | e, [] -> deQ([], List.rev e)
        | e1, e2 -> List.hd e2, (e1, List.tl e2)
  end


