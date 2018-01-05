(* complete *)
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
	type queue = (element list) * (element list)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (queue , element) = 
		match queue with ( [] , y) -> ([element] , y)
				|(x, y) -> (element::x , y)
	let rec deQ queue =
		match queue with
		(x,y) -> (match y with 
			(y1::y2) -> ( y1 , ( x , y2))
			| [] -> if x = [] then raise EMPTY_Q (* if second = null *)
				else deQ ( [] , List.rev x)  (* move first to second reversely*)
			)
end
;;
