module type Queue =
   sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
   end

module IntListQ : Queue with type element = int list =
struct
type element = int list
type queue = (element list * element list)
exception EMPTY_Q
let emptyQ = ([],[] : queue)
let enQ = fun (a,b : queue * element) -> match a with
		(c,d) -> (b::c,d : queue)
let deQ a = match a with
	([],[]) -> raise EMPTY_Q
	|(c,[]) -> (List.hd(List.rev c), ([], List.tl (List.rev c)) : element *queue)
	|(c,d) -> (List.hd d, (c, List.tl d) : element *queue)
end;;
