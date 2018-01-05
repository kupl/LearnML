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
  type queue = element list * element list
  exception EMPTY_Q
  
  let emptyQ = ([],[])

  let enQ ((stk_left,stk_right), new_ele) = (new_ele::stk_left, stk_right)

  let rec deQ q = 
   match q with 
    (stk_left,h::t) -> (h, (stk_left,t))
    |([],[]) -> raise EMPTY_Q
    |(stk_left,[]) -> (deQ ([],(List.rev stk_left)))
 end
