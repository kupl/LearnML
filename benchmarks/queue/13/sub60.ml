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
		type queue = (int list) list*(int list) list
		exception EMPTY_Q
		let rec rlswitch(q) =
			if (List.length (fst q)) == 0 then q
			else rlswitch(((List.tl (fst q)),((List.hd (fst q))::(snd q))))
		let emptyQ = ([],[])
		let enQ(q,e) = ((e::(fst q)),snd q)
		let rec deQ(q) =
			if (List.length (snd q)) == 0 then (
				if (List.length (fst q)) == 0 then raise EMPTY_Q
				else deQ(rlswitch(q))
			)
			else (List.hd (snd q),(fst q,List.tl (snd q)))
	end