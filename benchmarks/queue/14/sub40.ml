module type Queue = (*done done*)
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
	type queue = (element list) * (element list)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ ((q,s): queue * element) : queue = 
		if q = emptyQ then ([],[s])
		else (s::(fst q), snd q)
	let deQ (q: queue) : element * queue =
		match q with
		| [],[] -> raise EMPTY_Q
		| l, [] -> (List.hd (List.rev l), ([], List.tl (List.rev l)))
		| l, [s] -> (s, ([], List.rev l))
		| l, h::t -> (h, (l, t))		
	end
	
	
	
	
