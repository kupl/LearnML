(*
  CSE/2015-21233/김종권
  Homework 2-6
*)
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
  type queue = element * element
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ (q, e) =
    match q with
    | (l, r) -> (e :: l, r)
                
  let rec deQ q =
    match q with
    | ([], []) -> raise EMPTY_Q
    | (l, e::r) -> (e, (l,r))
    | (l, []) ->
      deQ ([], List.rev l)
end
