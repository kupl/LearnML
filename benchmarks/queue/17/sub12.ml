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
    let enQ (q,e) = match q with
      (l,r) -> (e::l,r)
    let deQ q = match q with
      (l,(hd::tl)) -> (hd,(l,tl))
      |(l,[]) -> match (List.rev l) with
        hd::tl -> (hd,([],tl))
        |[] -> raise (EMPTY_Q)
end
