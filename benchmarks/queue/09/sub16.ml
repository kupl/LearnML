module IntListQ :

sig
exception EMPTY_Q
type element = int list
type queue
val emptyQ : queue
val enQ : queue * element -> queue
val deQ: queue -> element * queue
end =

struct
exception EMPTY_Q
type element = int list
type queue = (element list)*(element list)
let emptyQ = ([],[])
let enQ((l1,l2),e) =
(e::l1,l2)

let deQ q = 
match q with
(a,[]) -> 	if a=[] then raise EMPTY_Q
		else
			let bh::bt=List.rev(a) in
			(bh,([],bt))|
(a,b::l) -> (b,(a,l))
end