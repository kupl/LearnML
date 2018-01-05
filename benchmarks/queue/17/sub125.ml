
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
  type queue = (element list) * (element list)
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ ((q : queue), (elt : int list)) = (
    match q with
        (l, r) -> (elt :: l, r)
  )
  let rec deQ q = match q with
      ([], []) -> raise EMPTY_Q
    | (l, []) -> (
        let rec rev i o = match i with
          | [] -> o
          | e :: t -> rev t (e :: o)
        in
          deQ ([], rev l [])
      )
    | (l, e :: t) -> (e, (l, t))
end
