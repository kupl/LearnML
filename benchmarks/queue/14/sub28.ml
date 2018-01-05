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
	type queue  = element list * element list
	exception EMPTY_Q

	let emptyQ = ([], [])
	let enQ ((q: queue), (e: element)): queue =
		match q with
		| (inS, outS) -> (e::inS, outS)
	let deQ (q: queue): element * queue = 
		match q with
		| (inS, outS) -> (
			if (inS, outS) = ([], [])
				then raise EMPTY_Q
			else if outS = []
				then let newOutS = List.rev inS in
			(List.hd newOutS, ([], List.tl newOutS))
			else
				(List.hd outS, (inS, List.tl outS))
		)
end

module ValidIntListQ = (IntListQ: Queue)