(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #6 *)
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
	let emptyQ : queue = ([], [])
	let enQ (p : queue * element) : queue =
		match p with
		| (q, e) ->
			match q with
			| (a, b) -> (e::a, b)
	
	let rec deQ (q : queue) : (element * queue) =
		match q with
		| ([], []) -> raise EMPTY_Q
		| (a, []) -> let b = List.rev(a) in deQ ([], b)
		| (a, h::b) -> (h, (a,b))
end
