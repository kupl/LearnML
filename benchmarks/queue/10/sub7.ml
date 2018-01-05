module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ : Queue with type element = int list =
struct
  type element = int list
  type queue = QUEUE of element list * element list
  exception EMPTY_Q
  let emptyQ = QUEUE([], [])
  let enQ (q, e) =
    match (q, e) with (QUEUE(a, b), h::t) -> QUEUE((h::t)::a, b)
    | (QUEUE(a, b), []) -> QUEUE([]::a, b)
  let deQ q =
    match q with QUEUE([], []) -> raise EMPTY_Q
    | QUEUE([], h::t) -> (h, QUEUE([], t))
    | QUEUE(a, []) -> ((List.hd (List.rev a)), QUEUE([], (List.tl (List.rev a)) ))
    | QUEUE(a, h::t) -> (h, QUEUE(a, t))
end