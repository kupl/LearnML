(* module signature definition *)
module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

(* module definition *)
module IntListQ = struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ : queue = [], []
  let enQ ((q : queue), (x : element)) : queue =
    match q with
    | lq, rq -> x::lq, rq
  let rec deQ (q : queue) : (element * queue) =
    match q with
    | [], [] -> raise EMPTY_Q
    | lq, [] -> deQ ([], List.rev lq)
    | lq, rq_hd::rq_tl -> rq_hd, (lq, rq_tl)
end
