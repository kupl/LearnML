module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end

module IntListQ  = struct
		type element = int list
		type queue = Queue of (int list) list * (int list) list
		exception EMPTY_Q
		let emptyQ = Queue([],[])

let rec revOne listOne newOne =
	match listOne with
	| [] -> newOne
	| h :: t -> revOne t (h :: newOne)

let req lis =  
	match Queue(lis, revOne lis []) with
	| Queue (a,b) ->
		match b with
		| [] -> ([], emptyQ)
		| h::t -> (h, Queue(revOne t [], t))


		let enQ = function
			| (que, elem) -> 
				match que with
				|	Queue(a,b) -> Queue(elem :: a, b)
		let deQ = function
			| Queue(a,b) -> req (a)
	end


