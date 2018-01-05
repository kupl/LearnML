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
    let emptyQ = ([],[])
    let enQ = fun ((el1,el2),e) -> (e::el1,el2)
    let rec deQ = fun q ->
      match q with
      | ([],[]) -> raise(EMPTY_Q)
      | (el,[]) -> deQ([],List.rev el)
      | (el1,e::el2) -> (e,(el1,el2))
  end
