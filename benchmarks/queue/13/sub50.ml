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
	let emptyQ : queue = ([],[])
	let enQ : queue * element -> queue = fun (myq, elem) -> 
		(elem::(fst myq) , (snd myq))
	let deQ : queue -> element * queue = fun myq ->
		let rec rev ( l, r ) = match (l, r) with
			| ([], right) -> ([], right)
			| (left, right) -> rev ( List.tl left, (List.hd left) :: right )
		in
		match myq with
			| ([],[]) -> raise EMPTY_Q
			| (left, []) -> let a = rev (left, []) in
					(List.hd (snd a), (fst a, List.tl (snd a))) 
			| (left, right) -> (List.hd right, (left, List.tl right))
  end
