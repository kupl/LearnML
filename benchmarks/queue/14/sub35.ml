
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
type queue = (element list)*(element list)
exception EMPTY_Q
let emptyQ =([],[]) 
let enQ (q,el)= 
	match q with
	| ([],[]) ->([el],[])
	| (l,r) -> (el::l,r)
let deQ q= 
	match q with
	| ([],[])->raise EMPTY_Q
	| (l,[])->(match (List.rev l) with 
			| h::t->(h,([],t))	
			)
	| (l,h::t)->(h,(l,t))
end
