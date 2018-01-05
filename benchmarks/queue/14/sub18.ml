(* hw2-5 *)

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
  let enQ ((q, e) : queue * element) : queue =
    (e::(fst q), snd q)
  let rec deQ (q : queue) : (element * queue) =
    match q with
    | ([], []) -> raise EMPTY_Q
    | (l, lh::lt) -> (lh, (l, lt))
    | (l, []) -> deQ ([], List.rev l)
end
