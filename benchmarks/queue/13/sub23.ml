(* 3 *)
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
      type queue = element * element
      exception EMPTY_Q
      let lQ q = (fst q)
      let rQ q = (snd q)
      let emptyQ = ([],[])
      let enQ (q, elem) = ((elem::(lQ q)), (rQ q))
      let rec deQ q = 
              match q with
              | ([], []) -> raise EMPTY_Q
              | (l, []) -> (deQ ([], (List.rev l)))
              | (l, h::t) -> (h, (l, t))
   end