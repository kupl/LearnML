module type Queue =
	sig
	type element
	type queue
	exception Empty_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
	end

module IntListQ =
	struct
	type element = int list
	type queue = element list * element list 
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ(queue', add) = (add::(fst queue'),snd queue') 
	let deQ queue' =
		if List.length (fst queue') == 0 && List.length (snd queue') == 0 then raise (EMPTY_Q)
		else if List.length (snd queue') == 0 then (List.hd (List.rev (fst queue')), ([],List.tl (List.rev (fst queue'))))
		else (List.hd (snd queue'), ((fst queue'), List.tl (snd queue')))
	end


