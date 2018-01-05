module type Queue = 
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element *queue
  end

module IntListQ = 
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ = function(myQ, lst) -> (lst::(fst myQ), (snd myQ))
    let deQ = function myQ ->
      if (myQ = emptyQ) then raise EMPTY_Q
      else
        if (not ((snd myQ) = []))
        then (List.hd (snd myQ), (fst myQ, List.tl (snd myQ)))
        else 
          let reverse = List.rev (fst myQ) in
            (List.hd reverse, ([], List.tl reverse))
  end
