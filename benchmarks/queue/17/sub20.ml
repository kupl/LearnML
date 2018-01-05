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

		type queue = int list list * int list list

		exception EMPTY_Q

		let emptyQ : queue = ([], [])

		let enQ : queue * element -> queue = fun (q, e) ->
			match q with
			|([], r) -> (e::[], r)
			|(l, []) -> (e::l, [])
			|(l, r) -> (e::l, r)

		let deQ : queue -> element * queue = fun q ->
			match q with
			|(l,[]) -> 
				(match ([], (List.rev l)) with
				 |(que, []) -> raise EMPTY_Q
				 |(que, hd::tl) -> (hd, (que,tl))
				)
			|([],head::tail) -> (head, ([],tail))
			|(l,head::tail)-> (head,(l,tail))
			
	end


