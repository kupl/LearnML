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
    type queue = int list list * int list list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (q, e) = 
      match q with
      | (l, r) -> ((e::l), r)
    let deQ q = 
      match q with
      | ([], []) -> raise EMPTY_Q
      | (l, []) -> 
          (let rev_l = (List.rev l) in
          match rev_l with
          | [] -> raise EMPTY_Q
          | hd::tl -> (hd, ([], tl)))
      | (l, hd::tl) -> (hd, (l, tl))
  end

(*제출 시 주석*)
(*module ValidIntListQ = (IntListQ: Queue)*)


