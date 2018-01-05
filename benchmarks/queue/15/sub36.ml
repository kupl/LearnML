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
		type queue = (int list) list * (int list) list
		exception EMPTY_Q
		
		let emptyQ = ([], [])
		
		let enQ : queue * element -> queue = 
			fun (q , e) -> (e::fst q, snd q) 
		
		let rec len l =
			match l with
			| [] -> 0
			| h::t -> 1 + len t
		
		let rec toss (l, r, n) =
			match (l, r, n) with
			| (_, [], _) -> (l, r)
			| (_, _, 0) -> (l, r)
			| (_, h::t, _) -> (fst(toss(h::l, t, n-1)), snd(toss(h::l, t, n-1)))

		let rec reverse l = fst(toss([], l, len l))
		
		let rec divideHalf l = 
			let halfLen = (len l) / 2 in
			let dividedPair = toss([], l, halfLen) in
			(reverse(fst dividedPair), reverse(snd dividedPair))
		
		let deQ : queue -> element * queue =
			fun q ->
				match q with
				| ([], []) -> raise(EMPTY_Q)(*(* empty queue *) ([], q)*)
				| (left, []) -> 
					let newLeft = fst(divideHalf left) in
					let newRight = snd(divideHalf left) in
					(match newRight with
						| [] -> ([], (newLeft, newRight))
						| h::t -> (h, (newLeft, t)))
				| (left, h::t) -> (h, (left, t))
				 
	end
