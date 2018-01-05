module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

module IntListQ =
struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ : queue = ([], [])
  let enQ ((target : queue), data) : queue = (data :: fst target, snd target)
  let rec deQ (target : queue) : element * queue =
    match target with
      ([], []) -> raise EMPTY_Q
    | (first, []) -> deQ ([], List.rev first)
    | _ -> (List.hd @@ snd target, (fst target, List.tl @@ snd target))
end
