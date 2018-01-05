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
		type element = int
		type queue = int list * int list (* 왼쪽이 넣는곳 오른쪽이 빼는곳 *)
		exception EMPTY_Q
		let emptyQ _ =
			([],[])
		let enQ qu ele =
			let (l1,l2) = qu in
			(ele::l1,l2)
		let deQ qu =
			let rec stack_reverse li =
				match li with
					[] -> []
					| x::y -> stack_reverse y @ [x] in
			match qu with
				([],[]) -> raise EMPTY_Q
				| (x,[]) -> 
					let de::rex = stack_reverse x in
					(de,([],rex))
				| (x,de::y) ->
					(de,(x,y))
	end
;;

