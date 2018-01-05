(* hw2-4 *)
(* 2010-11687 Keunjun Choi *)

module type Queue = sig
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
    let emptyQ: queue = ([], [])
    let enQ (que, e): queue = 
	match (que, e) with
	| ((l, r), elt) -> (elt::l, r)
    let deQ que: element * queue =
	let rec rdeQ q =
	    match q with
	    | ([], []) -> raise EMPTY_Q
	    | ([], r1::r2) -> (r1, ([], r2))
	    | (l, []) -> rdeQ ([], List.rev l)
	    | (l, r1::r2) -> (r1, (l, r2))
	in

	rdeQ que
end

module ValidIntListQ = (IntListQ: Queue)
