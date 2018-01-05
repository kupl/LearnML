module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end 

let moveL2R (queue) = 
  ([], (snd queue) @ (List.rev (fst queue))) ;;

module IntListQ =
  struct
    type element = int list
    type queue = element list * element list 
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (queue, element) = ([element] @ (fst queue), (snd queue))
    let deQ queue = 
      if queue = emptyQ
      then 
        raise EMPTY_Q
      else
        let moved_queue = (moveL2R queue) in
        ((List.hd (snd moved_queue)),
        ((fst moved_queue), (List.tl (snd moved_queue))))

  end;;

(* module ValidIntListQ = (IntListQ: Queue);; *)
