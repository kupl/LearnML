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
    let emptyQ = ([] , [])
    let enQ = fun (q,e) -> (e::(fst q),snd q)
    let deQ = fun q ->
      if (snd q) = [] then
	(if (fst q) = [] then raise EMPTY_Q
	else (List.hd (List.rev (fst q)),([],List.tl (List.rev (fst q)))))
      else (List.hd (snd q),(fst q,List.tl (snd q)))
  end
