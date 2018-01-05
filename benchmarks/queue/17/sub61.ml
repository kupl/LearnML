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
    let enQ ((q:queue), (e:element)) : queue =
      (e::(fst q), (snd q))
    let deQ (q:queue) : (element * queue) =
      let (l, r) = q in
        (if q = ([], []) then raise EMPTY_Q
        else if r = [] then ((List.hd (List.rev l)), ([], (List.tl (List.rev l))))
        else ((List.hd r), (l, (List.tl r)))
        )
  end
