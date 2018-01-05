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
let emptyQ = ([], [])
let enQ(q, e): queue = (e::fst q, snd q)
let deQ q = match q with
	| (fore, e::back) -> (e, (fore, back))
	| (fore, []) -> match List.rev fore with
		| [] -> raise EMPTY_Q 
		| e::back -> (e, ([], back))
end

(* module ValidIntListQ = (IntListQ : Queue) *)