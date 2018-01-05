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
  let emptyQ = ([],[])
  let enQ(q, ele) = 
  match q with
  (a, b) -> (ele::a, b)
  let deQ q = 
  match q with
  ([], []) -> raise EMPTY_Q
  | (h::t, []) -> (h, ([], List.rev t))
  | (a, h::t) -> (h, (a, t))
end


  
