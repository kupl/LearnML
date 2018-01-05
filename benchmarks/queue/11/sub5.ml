(*
	2006-11681 강현석
	hw2 : exercise 5
*)

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
	type queue =  element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (q,e) = 
		match q with
		(l,hr::tr) -> (e::l,hr::tr)
		| (l,[]) -> (e::l,[])
	let rec deQ q = 
		(if q = emptyQ then raise EMPTY_Q
		 else (match q with
				(l,r) -> (match r with
					  	  h::t -> (h,(l,t))
						  | [] -> (deQ ([],List.rev l)))
							    ))
end
		

