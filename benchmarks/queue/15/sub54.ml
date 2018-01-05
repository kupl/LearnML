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
    type queue = element * element
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (q, elem) =
      match q with
      | (l, r) -> (elem@l, r)
    let rec deQ q =
      match q with
      | (l, r::r_lst) -> (r::[], (l, r_lst))
      | ([], []) -> raise EMPTY_Q
      | (l, []) -> (deQ ([], List.rev l))
  end
