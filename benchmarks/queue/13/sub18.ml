module type Queue = 
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end

module IntListQ =
struct
	type element = int list
	type queue = ((int list) list * (int list) list)
	
	exception EMPTY_Q
	
	let emptyQ = ([],[])
	
	let enQ (q, e) = 
	match q with
	| ([] , b ) -> ([e] , b)
	| (a , []) -> ([e] , List.rev a)
	| (a , b) -> ( e::a , b)
	
	let rec deQ q =
	match q with
	| ([] ,[]) -> raise EMPTY_Q
	| (hd::[] , [] ) -> (hd, ([],[]))
	| (a , [] ) -> deQ ([], List.rev a)
	| (a , hd::tl) -> (hd, (a , tl))
end
