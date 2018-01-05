
module type Queue =
sig
type element
type queue
exception EMPTYQ
val emptyQ: queue
val enQ: queue * element -> queue
val deQ: queue -> element * queue
end

module IntListQ =
 struct
  type element = int list
  type queue = element * element
  exception EMPTYQ  
  let emptyQ = ([],[])
  
  let enQ (q, e) = match q with
  | (l, r) -> (e@l, r)
  
  let rec deQ q =   
  let rec rev l = match l with
  | [] -> []
  | h::t -> rev t@[h] in  
  match q with
  | ([], []) -> raise EMPTYQ
  | (l, []) -> deQ ([], (rev l))
  | (_, h::r) -> h 
  
 end
