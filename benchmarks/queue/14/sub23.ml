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
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
		
	let enQ (q, e) =
		(e::(fst q), (snd q))
	let deQ q =
		match q with
		| ([], []) -> raise EMPTY_Q
		| (_, []) -> ((List.hd (List.rev (fst q))), ([], (List.tl (List.rev (fst q)))))
		| _ -> ((List.hd (snd q)), ((fst q), (List.tl (snd q)))) 
end

(* module ValidIntListQ = (IntListQ: Queue) *)
