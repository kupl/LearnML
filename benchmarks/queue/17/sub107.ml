module type Queue = 
sig
type element
type queue
exception EMPTY_Q
val emptyQ : queue
val enQ : queue * element -> queue
val deQ : queue -> element*queue
end

module IntListQ = 
struct
type element = int list
type queue = (int list list * int list list)
exception EMPTY_Q
let emptyQ = ([],[])


let enQ ((q1 : queue),(e1 : element)) : queue = 
match (q1,e1) with
|(([],l2),e) -> ([e],l2)
|((l1,l2),e) -> (e::l1,l2)



let deQ (q2 : queue) : (element * queue) = 
match (q2) with
([],_) -> raise (EMPTY_Q)
|(l1,l2) -> (List.hd (List.rev l1), (List.rev(List.tl(List.rev l1)) , List.tl(List.rev l1)))


end

