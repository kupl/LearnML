(* 2009-11824 Jieun-Jeong HW2-5 *)

module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end

module IntListQ : Queue with type element = int list = 
struct 
   type element = int list
   type queue = element list * element list
   exception EMPTY_Q
   let emptyQ = ([], [])
   let enQ (q, elm) =
   		match q with
		|(a, b)	-> (elm::a, b)
   let rec deQ q =
   		let rec refresh q =
			match q with
			|([], _)	-> q
			|(e::l, b)	-> (refresh (l, e::b))
		in
   		match q with
		|([], [])	-> raise EMPTY_Q
		|(_, [])	-> (deQ (refresh q))
		|(a, e::l)	-> (e, (a, l))
end 
