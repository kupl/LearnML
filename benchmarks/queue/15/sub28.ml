(* hw2ex6.ml *)



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
  type queue = (element list) * (element list)
  exception EMPTY_Q
  let emptyQ = ([],[])
  let enQ = fun ((q,e) : queue * element) -> 
    if q = emptyQ then ([e],[])
    else (e::(fst q),snd q)
  let deQ = fun (q: queue) ->
    match q with
      | [], [] -> raise EMPTY_Q
      | l, [] ->
          (List.hd (List.rev l), ([],List.tl (List.rev l)))
      | l, [s] -> (s, ([], List.rev l))
      | l, h::t -> (h, (l,t))
end



(* testcase

  module ValidIntListQ = (IntListQ : Queue)



  let myQ = IntListQ.emptyQ
  let yourQ = IntListQ.enQ(myQ, [1])
  let (x,restQ) = IntListQ.deQ yourQ
  let hisQ = IntListQ.enQ(myQ, [2])

  let q1 = IntListQ.emptyQ 
  let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6]) 
  let (e1, q3) = IntListQ.deQ q2    ;; 

*)

