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
  let emptyQ: queue = ([], [])
  let enQ(queue, element: queue * element): queue =
    match queue with
      (q1, q2) -> (element :: q1, q2)
  let rec deQ(queue: queue): element * queue =
    match queue with
      (l, rh::rt) -> (rh, (l, rt))
    | ([], []) -> raise EMPTY_Q
    | (l, []) -> deQ([], List.rev(l))
end
