exception EMPTY_Q of string

module type Queue = 
sig
 exception EMPTY_Q
 type element
 type queue

 val emptyQ: queue
 val enQ: queue * element -> queue
 val deQ: queue -> element * queue
end

module IntListQ =
struct
  exception EMPTY_Q
  type element = int list
  type queue = int list list * int list list
                 
  let emptyQ = ([], [])
               
  let enQ ((q: queue), (e:element)) : queue  = (e :: fst q, snd q)
  let deQ (q:queue) : (element * queue) =
    match q with
    | (_, h :: t) -> (h, (fst q, t))
    | (h :: t, []) -> 
      let rev_l = List.rev (fst(q)) in
      let hd_l = List.hd rev_l in
      let t_l = List.tl rev_l in
      (hd_l, ([], t_l))
    | ([], []) -> raise EMPTY_Q ("EMPTY QUEUE")
             
end





