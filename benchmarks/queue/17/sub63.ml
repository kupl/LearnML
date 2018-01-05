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
    let enQ = fun ((l, r), e) -> (e::l, r)
    let rec deQ = fun (l, r) ->
      match l, r with
        | [], [] -> raise EMPTY_Q
        | _, [] -> deQ([], List.rev l)
        | _, b::tr -> b, (l, tr)
  end
