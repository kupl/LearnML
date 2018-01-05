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
    type queue = QUEUE of element list * element list
    exception EMPTY_Q
    let emptyQ = QUEUE ([], [])
    let enQ (q, e) = 
      match q with
        QUEUE (l, r) -> QUEUE(e::l, r)

    let rec deQ q = 
      match q with
       QUEUE ([], []) -> raise EMPTY_Q
       | QUEUE (l, r) ->
        let popRight qq = 
          match qq with
            QUEUE (ll, []) -> deQ (QUEUE([], List.rev ll))
            | QUEUE (ll, e::rr)  -> (e, QUEUE (ll, rr)) in
        popRight (QUEUE (l, r))
  end

(*
let (x, a) = IntListQ.deQ(IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(IntListQ.emptyQ, [1;2]), [2;3;4]), [3;4;5;6]), [2;3]));; 
let a = IntListQ.enQ(a, [3;1]);; 

let a = IntListQ.enQ(a, [3;2]);; 

let (x, a) = IntListQ.deQ(a);; 

let (x, a) = IntListQ.deQ(a);; 

let (x, a) = IntListQ.deQ(a);; 

let (x, a) = IntListQ.deQ(a);; 

let (x, a) = IntListQ.deQ(a);; 

let (x, a) = IntListQ.deQ(a);; 

let (x, a) = IntListQ.deQ(a);; 
*)
