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
    let emptyQ = [], []
    let enQ : queue * element -> queue = fun (q, e) ->
      let (la , lb) = q in
      (e::la, lb)
    let rec deQ : queue -> element * queue = fun q ->
      match q with
      | [], [] -> raise EMPTY_Q
      | l , [] -> deQ ([] , List.rev l)
      | l , hd::tl -> (hd, (l, tl))
end
