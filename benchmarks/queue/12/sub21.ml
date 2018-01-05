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
    let enQ : queue * element -> queue = fun (q, ele) ->
      match q with
      | (lq, rq) -> (ele::lq, rq)
    let deQ : queue -> element * queue = fun(q) -> 
      match q with
      | ([], []) -> raise EMPTY_Q
      | (lq, []) -> (List.hd(List.rev(lq)), ([], List.tl(List.rev(lq))))
      | (lq, rq) -> (List.hd(rq), (lq, List.tl(rq)))

  end
