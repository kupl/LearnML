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
let emptyQ = ([],[])
let enQ (q, el) = ([el]@(fst q), (snd q)) 
let deQ q = 
	match q with
		([],[]) -> raise EMPTY_Q
		|(f,[]) -> ((List.hd (List.rev f)), ([], (List.tl (List.rev f))))
		|(f, s) -> ((List.hd s), (f, (List.tl s))) 
end

