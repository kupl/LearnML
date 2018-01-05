(* hw 2-5 *)
(* 2012-11269 DongJae Lim *)

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
    let emptyQ = ([], []) 
    let enQ ((q : queue), (e : element)) : queue =
      let (inq, deq) = q in
      (e::inq, deq)
    let deQ (q : queue) : element * queue =
      let (inq, deq) = q in
      match deq with
      | [] -> 
        (let ndeq = deq @ (List.rev inq) in
         match ndeq with
         | [] -> raise EMPTY_Q
         | dh::dt -> (dh, ([], dt)))
      | dh::dt -> (dh, (inq, dt))
  end
