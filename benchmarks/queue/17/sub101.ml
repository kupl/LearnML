(* 2015-11380 박찬양 HW2_6 *)

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
    let enQ: queue * element -> queue = fun (q, e) ->
      match q with
      | (a, b) -> (e::a, b)
    let deQ = fun q ->
      match q with
      | ([], []) -> raise EMPTY_Q
      | ([], h::t) -> (h, ([], t))
      | (a, []) -> 
        let h::t = List.rev a in
        (h, ([], t))
      | (a, h::t) -> (h, (a, t)) 
  end

  