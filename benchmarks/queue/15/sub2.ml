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
    type queue = (element list * element list)
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (target_queue, new_element) =
      match target_queue with
      | (a, b) -> (new_element::a , b)
    let rec deQ target_queue =
      match target_queue with
      | ([], []) -> raise EMPTY_Q
      | (a, []) -> deQ (([], List.rev a))
      | (a, b_head::b_tail) -> (b_head, (a, b_tail))
  end
