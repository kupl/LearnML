(* 2012-11230 Kim sangmin *)

module type Queue =
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
		let enQ = fun(que, elem) -> ((elem::fst(que)), snd(que))			
		let deQ = fun(que) -> 
			match que with
			| ([], []) -> raise(EMPTY_Q)
			| (first, []) -> (
					let tmp = List.rev(first) in
					(List.hd(tmp), ([], List.tl(tmp)))
					)
			| (first, second) -> (List.hd(second),(first, List.tl(second)))

	end
