(* HW 2-5 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

module type Queue = 
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ : Queue with type element = int list = 
struct 
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (q, x) = match q with
			  (l, r) -> (x::l, r)
	let deQ q = match q with
			(l, r) -> match r with
					[] -> ( match (List.rev l) with
						 [] -> raise EMPTY_Q
						 | h::t -> (h, ([], t)) )
					| h::t -> (h, (l, t))
end 
