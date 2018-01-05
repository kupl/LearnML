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
    let enQ(q, elem) = let (l_list, r_list) = q in ((elem::l_list, r_list))
    let deQ(q) = let (l_list, r_list) = q in match r_list with
    [] -> ( match (List.rev l_list) with
      [] -> raise EMPTY_Q
      | hd::tl -> (hd, ([], tl)) )
    | hd::tl -> (hd, (l_list, tl))
  end
