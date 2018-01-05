(*2016-11690*)
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
		type queue = (int list) list * (int list) list (*https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=4969*)
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ (q,elem) =
			let l = fst q in
			let r = snd q in
			( elem::l, r)
			
			(*
			match q with
			| (L,R) -> ( elem::L ,R)
			*)

		let deQ q =
			match q with
			| ([],[]) -> raise EMPTY_Q
			| (l,[]) -> let r = List.rev l
				in (List.hd r, ([],List.tl r) )
			| (l,elem::r) -> (elem,(l,r))
	end

(*need to be erased before submission *) (* module ValidIntListQ = (IntListQ: Queue) *)