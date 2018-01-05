(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-6.ml *)

module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ : queue
    val enQ : queue * element -> queue
    val deQ : queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = (int list) list * (int list) list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ = fun ((l, r), x) -> (x::l, r)
    let rec deQ = fun q ->
      match q with
      | ([], []) -> raise EMPTY_Q
      | (l, []) -> deQ ([], List.rev l)
      | (l, rh::r) -> (rh, (l, r))
  end

(* module ValidIntListQ = (IntListQ : Queue) *)
