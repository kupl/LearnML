module type Queue = 
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ  =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, item) = (item::(fst q), snd q)
		let deQ q =
			let q' = (if List.length (snd q) = 0 then ([], List.rev (fst q)) else q) in
			if List.length (snd q') = 0 then raise (EMPTY_Q)
			else (List.hd (snd q'), (fst q', List.tl (snd q')))
	end

