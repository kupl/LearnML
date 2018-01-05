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
type queue = element list * element list
exception EMPTYQ
let emptyQ = ([], [])
let enQ = fun (q, e) -> match q with
			| (ls, rs) -> (e::ls, rs)
let deQ = fun q -> match q with
		   | (ls, rs) -> match rs with
				 | [] -> ((List.nth ls ((List.length ls) - 1)), ([], (List.rev (List.tl ls))))
				 | hd::tl -> (hd , (ls, tl))
end
