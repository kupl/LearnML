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
      type element= int list
    type queue= int list list * int list list
    exception EMPTY_Q
    let emptyQ= ([], [])
    let enQ= fun(q, l) ->
      match q with
      | (l1, l2) -> (l::l1, l2)
    let deQ= fun q->
      match q with
      | ([], [])-> raise EMPTY_Q
      | (l1, [])-> 
          ( match (List.rev l1) with 
          | [] ->(raise EMPTY_Q)
          | (hd::tl)-> ([], tl) )
      | (l1, hd::tl)-> (l1, tl)
    end

(*
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])

*)
