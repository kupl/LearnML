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
	type queue = int list list*int list list
	exception EMPTY_Q
	
	let emptyQ = ([],[])
	
	let enQ = fun (q, e) ->
		match q with (ql, qr) -> (e::ql, qr)

	let rec reverseL = fun (d, r) ->
		match r with 
		|[] -> d
		|e::rl -> reverseL (e::d, rl)
	
	let rec restructQ = fun q ->
		match q with (ql, qr) ->
			match ql with 
			|[] -> ([],qr)
			|e::leftql -> restructQ (leftql, e::qr)
	
	let rec deQ = fun q -> 
		match q with (ql, qr) ->
		(match qr with 
		|head::leftqr -> (head, (ql, leftqr))
		|[] -> (match ql with
			|[] -> raise EMPTY_Q
			|_ ->deQ (restructQ q))
		)
	end

