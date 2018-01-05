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
    let emptyQ = ([], [])
    let enQ (q, elem) = (elem::(fst q), snd q)
    let deQ q =
      match q with
      | ([], []) -> raise EMPTY_Q
      | (ls, []) -> (List.hd (List.rev ls), ([], List.tl (List.rev ls)))
      | (ls1, ls2) -> (List.hd ls2, (ls1, List.tl ls2))
  end
