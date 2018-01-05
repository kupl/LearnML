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
  type queue = (int list) list * (int list) list
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ : queue * element -> queue = fun ((l,r),el) -> (el::l,r)
  let rec deQ : queue -> element * queue = fun q -> match q with
  |(le,r::ri) -> (r,(le,ri))
  |(l::left,[]) -> deQ(moveLR(l,left,emptyQ))
  |emptyQ -> raise EMPTY_Q
  and moveLR : element * (int list) list * queue -> queue = fun (l,li,q) -> match (l,li,q) with
  |(l,[],(le,ri)) -> (le,l::ri)
  |(l1,l2::left,(le,ri)) -> moveLR(l2,left,(le,l1::ri)) 
 end 
