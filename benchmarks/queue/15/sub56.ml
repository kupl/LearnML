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
  type queue = (element list) * (element list)
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ: queue * element -> queue = fun ((l1, l2), e) -> (e::l1, l2)
  let deQ: queue -> element * queue = fun (l1, l2) ->
    match (l1, l2) with
    | ([], []) -> raise EMPTY_Q
    | (l1, []) -> begin
      match List.rev l1 with
      | [] -> raise EMPTY_Q
      | h::t -> (h, ([], t))
      end
    | (l1, h::t) -> (h, (l1, t))
end
