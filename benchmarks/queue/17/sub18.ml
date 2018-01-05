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
  let emptyQ = ([], [])
  let enQ ((q : queue), (l : int list)) = 
    match q with
    | (l1, l2) -> ((List.append [l] l1), l2)
  let rec deQ (q : queue) = 
    match q with 
    | ([], []) -> raise (EMPTY_Q)
    | (l1, []) -> deQ([], (List.rev l1))
    | (l1, hd::tl) -> (hd, (l1, tl))  
end

