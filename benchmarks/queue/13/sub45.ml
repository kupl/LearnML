(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
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

module IntListQ = struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (orgnQ, elem) =		(* queue * element -> queue *)
		((elem::fst orgnQ), snd orgnQ)
	let rec deQ orgnQ =
		match orgnQ with
		| ([], []) -> raise EMPTY_Q
		| (lst ,[]) -> deQ ([], (List.rev lst))
		| (lst, l::t) -> (l, (lst, t))
end
