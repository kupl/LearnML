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
		let emptyQ = ([], [])
		let enQ = fun ( (l, r) ,  e) -> (e::l, r)
		let deQ = fun q -> match q  with
						   | ([],[])-> raise EMPTY_Q
						   | (l , r) -> (match r with
						   				| [] -> (match (List.rev l) with
												[] -> raise EMPTY_Q
												| (hl::tl)-> (hl, ([], tl)))

										| (hr::tr) ->(hr, (l, tr))) 
	end

module ValidIntListQ =  (IntListQ: Queue)

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])

