module type Queue = 
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element ->queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = ((int list) list *((int list) list))
    exception EMPTY_Q

    let emptyQ: queue = ([], [])
    let enQ (((l, r) : queue), (x: element)): queue =
      (x::l, r)

    let revList (l : (int list) list): (int list) list = 
      List.rev l

    let deQ ((l, r): queue): element * queue = 
      match (l, r) with
      | (_, []) -> (try (List.hd (revList l)), ([], List.tl (revList l))
                    with Failure _ -> raise EMPTY_Q)
      | (l, r) -> (List.hd r, (l, List.tl r))
  end
