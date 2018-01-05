
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
		let emptyQ = ([], [])
		let enQ (que_en, elm_target) =
			(
			match que_en with
			| (l, r) -> (elm_target::l, r)
			)
		let deQ (que_de) =
			(
			match que_de with
			| ([], []) -> raise EMPTY_Q
			| (l, []) ->
			 	let r = List.rev l in
				let r_hd = List.hd r in
				let r_tl = List.tl r in
			 	(r_hd, ([], r_tl))
			| (l, r_hd::r_tl) -> (r_hd, (l, r_tl))
			)
	end
(*
module ValidIntListQ = (IntListQ: Queue)
*)


