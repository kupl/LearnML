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

    (* Make an empty queue *)
    let emptyQ: queue = ([],[])

    (* Insert element to queue *)
    let enQ ((q: queue), (e: element)): queue =
      match q with
      | (stack1, stack2) -> (e::stack1, stack2)

    (* Delete element from queue *)
    let rec deQ (q: queue): (element * queue) =
      match q with
      | ([], []) -> raise EMPTY_Q
      | (stack1, []) -> deQ (([], List.rev stack1))
      | (stack1, stack2) -> (List.hd stack2, (stack1, List.tl stack2))
end
