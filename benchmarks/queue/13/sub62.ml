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
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ(q,e)=
		match q with
		|(a,b) -> if b=[] then ([],List.append (List.rev a) (e::[]))
				else (List.append (e::[]) a,b)
	let deQ q=
		match q with
		|(_,[]) -> raise EMPTY_Q
		|(a,hd::tl) -> if tl=[] then (hd,([],List.rev a))
						else (hd,(a,tl))
end

