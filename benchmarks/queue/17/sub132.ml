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
    let emptyQ : queue = ([], [])
    let rec enQ : queue * element -> queue = fun ((l1, l2), e) -> (e::l1, l2)
    let rec deQ : queue -> element * queue = function
      | ([], []) -> raise EMPTY_Q
      | (l1, []) -> deQ ([], (List.rev l1))
      | (l1, hd::tl) -> (hd, (l1, tl))
  end
