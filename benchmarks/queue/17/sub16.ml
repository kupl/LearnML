module type Queue = sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ = struct
  type element = int list
  type queue = int list list * int list list
  exception EMPTY_Q
  let emptyQ : queue = ([], [])
  let enQ : queue * element -> queue = fun (q, e) ->
    match q with
    | (x, y) -> (e :: x, y)
  let rec deQ : queue -> element * queue = fun q ->
    match q with
    | ([], []) -> raise (EMPTY_Q)
    | (x, []) -> deQ([], List.rev x)
    | (x, y) ->
      (match y with
       | [] -> raise (EMPTY_Q)
       | head :: tail -> (head, (x, tail)))

end
