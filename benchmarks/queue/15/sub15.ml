(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-6 *)

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
    let enQ = fun(q, e) ->
      match q with
      | l, r -> e::l, r
    let deQ = fun(q) ->
      let rec subDeQ = fun(sq) ->
        match sq with
        | ([], []) -> raise EMPTY_Q
        | (l, []) -> subDeQ(([], List.rev(l)))
        | (l, hd::tl) -> (hd, (l, tl)) in
      subDeQ(q)
  end

  (*
  module ValidIntListQ = (IntListQ : Queue)

    let q1 = IntListQ.emptyQ 
    let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6]) 
    let (e1, q3) = IntListQ.deQ q2    ;; 

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])

let q1 = IntListQ.emptyQ 
let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6]) 
let (e1, q3) = IntListQ.deQ q2    ;; 
    *)
