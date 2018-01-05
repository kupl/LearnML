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
  type queue = int list list * int list list
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ : queue * element -> queue = fun q_elem ->
    match q_elem with
    | ((f, t), element) -> (element::f, t)
  let rec deQ : queue -> (element * queue) = fun q ->
    match q with
    | ([], []) -> raise EMPTY_Q
    | (f, []) -> deQ([], List.rev f)
    | (f, hd::tl) -> (hd, (f, tl))
end
