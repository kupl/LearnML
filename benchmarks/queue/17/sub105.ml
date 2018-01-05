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
    let emptyQ : queue = ([], [])
    let enQ (q, e) = (e::(fst q), snd q)
    let rec deQ q =
      match snd q with
      | hd::tl -> (hd, (fst q, tl))
      | [] ->
        if fst q = [] then raise EMPTY_Q
        else deQ([], List.rev(fst q))
  end

(*let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let yourQ = IntListQ.enQ(yourQ, [3])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])
let (y,restQ) = IntListQ.deQ restQ
let () = Printf.printf "%d" (List.hd x)
let () = Printf.printf "%d" (List.hd y)
*)

