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
  let enQ ((q : queue), (elt : element)) : queue =
    match q with
    | (l1, l2) -> (elt :: l1, l2)
  
  let deQ (q : queue) : element * queue =
    match q with
    | ([], []) -> raise(EMPTY_Q)
    | (l1, []) -> (
      let revl1 = List.rev(l1) in
      (List.hd(revl1), ([], List.tl(revl1)))
    )
    | (l1, h2::t2) -> (h2, (l1, t2))
end