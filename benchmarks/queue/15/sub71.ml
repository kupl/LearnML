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
		let emptyQ = ([],[])
		let enQ= fun(q,el) -> (el::(fst q) , (snd q))
		let deQ = fun(q) -> 
		match q with
		| ([],[]) -> raise EMPTY_Q
		| (l,[]) -> (List.hd(List.rev(l)),([],List.tl(List.rev(l))))
		| (_,_) -> (List.hd(snd q),((fst q),List.tl(snd q)))
	end
