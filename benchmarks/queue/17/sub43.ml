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
    type queue = ((int list) list) * ((int list) list)
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (q, e) = (e::(fst q), (snd q))
    let deQ q =
      match q with
      | ([], []) -> raise (EMPTY_Q)
      | (l1, []) -> (List.hd (List.rev l1), ([], List.tl (List.rev l1)))
      | _ -> ((List.hd (snd q)), ((fst q), List.tl (snd q)))
  end