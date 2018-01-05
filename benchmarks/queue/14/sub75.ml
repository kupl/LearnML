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
	let enQ = fun (q, e) ->
	match q with
	| (left, right) -> (e::left,right)
	let deQ = fun q -> 
	match q with
	| (left, []) ->  ( List.hd (List.rev left), ([], List.tl (List.rev left)))
	| (left, right) -> (List.hd right, (left, List.tl right))

  end

 