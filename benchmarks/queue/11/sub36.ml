module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

module IntListQ : Queue with type element = int list =
struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ ((l,r),e) = (e::l,r)
  let rec deQ q = match q with
    | ([],[]) -> raise EMPTY_Q
    | (h::l,r) -> (h,(l,r))
    | ([],r) -> deQ (List.rev r,[])
end
(*
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2]) *)
