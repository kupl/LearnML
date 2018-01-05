module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end;;
module IntListQ =
struct
  type element = int list
  type queue = (element list*element list)
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ = fun (queue,element) -> match queue
  with (l1,l2) -> (element::l1, l2)
  let deQ = fun queue -> match queue
  with ([],[]) -> raise (EMPTY_Q)
    | (l1,[]) -> ((List.hd (List.rev l1)), ([], List.tl (List.rev l1)))
    | (l1,l2) -> ((List.hd l2), (l1,List.tl l2))
end
