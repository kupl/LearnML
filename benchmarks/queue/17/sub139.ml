(* 컴퓨터공학과/2017-34165/김성국/2-6 *)
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
  type queue = element list * element list
  exception EMPTY_Q

  let emptyQ = ([], [])
  let enQ ((l,r), e) = (e::l, r)
  let deQ (l, r) =
    match (l, r) with
    | ([], []) -> raise EMPTY_Q
    | (l, []) -> let r = (List.rev l) in ((List.hd r), ([], List.tl r))
    | (l, f::rr) -> (f, (l, rr))
end
