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
    let enQ = fun (q, e) -> (e :: fst(q), snd(q))
    let deQ = fun (q) ->
      match q with
      | ([], []) -> raise EMPTY_Q
      | (a, []) -> (List.hd(List.rev a), ([], List.tl(List.rev a)))
      | (a, hd :: tl) -> (hd, (a, tl))
  end