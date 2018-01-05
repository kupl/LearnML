module IntListQ =
	struct
		type element = int list
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ : queue = ([], [])
		
		let enQ : queue * element -> queue =  fun (q , elm )  -> 
			match q with
			|  (l, r) ->
				  (elm::l, r)
		
    let deQ (q : queue) : element * queue =
			match q with
			| (l, r) ->
				(match r with
				| [] -> 
					(match l with
					| [] -> raise EMPTY_Q
					| _  -> let ver = List.rev l in
					      (match ver with
								| [] -> raise EMPTY_Q
								| hd::tl -> (hd, ([], tl)))) 
				| hd::tl -> (hd, (l, tl)))
	
	end