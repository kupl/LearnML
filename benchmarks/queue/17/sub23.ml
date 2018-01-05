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
    let emptyQ : queue = ([],[])
    let enQ : queue * element -> queue =
      fun ((l1,l2), e) -> (e::l1,l2)
    let deQ : queue -> element * queue = fun (l1, l2) -> 
      match (l1, l2) with
      |([],[]) -> raise EMPTY_Q
      |(l1, []) -> (List.hd(List.rev(l1)), ([],List.tl(List.rev(l1))))
      |(l_, hd::tl) -> (hd, (l1, tl))
  end
