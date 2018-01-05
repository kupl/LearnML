
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
		let emptyQ = ([],[])
		let enQ ((q:queue), (e:element)) =
			match q with
			|(a,b) -> (e::a, b)
		let rec deQ ((q:queue)) = 
			let rec dump(p:queue) =
				match p with
				|([], n) -> ([], n)
				|(h::t, n) -> dump(t, h::n)
			in
			match q with
			|([],[]) -> raise(EMPTY_Q)
			|(a, []) -> deQ(dump(q))
			|(a, h::t) -> (h, q)
	
	end		

