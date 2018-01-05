module type Queue=
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue*element -> queue
	val deQ: queue -> element*queue
end


module IntListQ  = 
struct
	type element = int list
	type queue = element list * element list

	exception EMPTY_Q

	let emptyQ = ([],[])

	(* enQ *)
	let enQ (q, e) =
		match q with
			| (inbox, outbox) -> ( e::inbox , outbox)

	(* deQ *)
	let deQ q = 
		let (*rec*) aux = function
			| ( [], outbox ) as que -> que
			| ( inbox, outbox ) -> ( [], List.rev inbox @ outbox )
			(*| ( ihd::itl, outbox ) -> aux ( itl, ihd::outbox ) *)
		in
		match q with
			| ([],[]) -> raise EMPTY_Q
			| (inbox, hd::tl) -> ( hd , (inbox, tl))
			| ( _ , []) as que -> 
				let (newinbox, newoutbox) = aux que in
					if newoutbox = [] then raise EMPTY_Q else ( List.hd newoutbox, (newinbox, List.tl newoutbox))
end


(* Code to check whether IntListQ satisfies Queue module implementation *)
(* Need to be commented before submitting ! *)
(* module ValidIntListQ = (IntListQ : Queue ) *)
(*
open IntListQ

let myQ = enQ( emptyQ, [1] )
let yourQ = enQ( myQ, [2;3] )
let q3 = enQ( yourQ, [4;5] )
let (pop, rest) = deQ q3
let _= List.iter (print_int) pop
*)
