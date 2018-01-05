(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercise 6
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

let rec listrev((input: int list list), (output: int list list)): int list list =
	match input with
	| [] -> output
	| (hd::tl) -> listrev(tl, hd::output)


module IntListQ =
	struct
		type element = int list
		type queue = element list * element list
(*(int list) list * (int list) list*)
		exception EMPTY_Q
		let emptyQ: queue = ([], [])
		let enQ((q: queue), (e: element)): queue = 
			match q with
			| ([], tl) -> ([e], tl)
			| (hd, tl) -> (e::hd, tl)
(*			((e::List.hd q), (List.tl q)) *)
		let rec deQ(q: queue): element * queue =
			match q with
			| ([], []) -> raise EMPTY_Q
			| (biglist, []) -> deQ(([], listrev(biglist, [])))
			| (biglist, hd::tl) -> (hd, (biglist, tl))
	end

