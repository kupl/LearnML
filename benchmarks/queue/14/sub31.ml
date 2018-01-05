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
  type queue=(int list list*int list list)
  exception EMPTY_Q
  let emptyQ= ([],[])
  let enQ (q,ele) =
   match q with
     | (a,b) -> (a,ele::b)
  let rec deQ (q:queue) = 
    match q with
     | ([],[]) -> raise EMPTY_Q
     | (h::t,l) -> (h,(t,l))
     | ([], l) -> deQ (List.rev_append l [],[])
end