module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ (* : Queue with type element = int list *) =
struct
  type element = int list
  type queue = (int list) list * (int list) list
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ ((list_1, list_2), e) = 
    (e::list_1 , list_2)

  let deQ (list_1, list_2) =
    match list_2 with
    |[] ->
      if List.length list_1 = 0 then
        raise EMPTY_Q
      else
        let new_list_2 = List.rev list_1 in
        let deq_elem = List.hd new_list_2 in
        let new_queue = ([], List.tl new_list_2) in
        (deq_elem, new_queue)
    |hd::tl -> (hd, (list_1, tl))
    
end

(*
module CheckModule = (IntListQ: Queue)
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])
*)
