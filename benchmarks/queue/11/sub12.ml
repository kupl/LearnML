(* 2006-11867 Jo, Dong-Chul *)

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
type queue = element list * element list
exception EMPTY_Q
let emptyQ = ([], [])
let enQ = fun (q, e) ->
	match q with
	| (ll, rl) -> (e::ll, rl)
let deQ = fun q ->
	match q with
	| ([], []) -> raise (EMPTY_Q)
	| (ll, []) ->
		let revl = List.rev ll
		in
		(
			match revl with
			| [] -> raise (EMPTY_Q)
			| e::rl -> (e, ([], rl))
		)
	| (ll, e::rl) -> (e, (ll, rl))
end
