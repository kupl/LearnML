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
    let emptyQ : queue = ([], [])
    let enQ : queue * element -> queue 
    = fun params ->
      match params with
      | (q, elem) -> ((elem::(fst q)), (snd q))
    let rec deQ : queue -> element * queue
    = fun q ->
      match q with
      | ([], []) -> raise EMPTY_Q
      | (left, []) -> (deQ ([], (List.rev left)))      
      | (_, d::right) -> (d, ((fst q), right))
  end
