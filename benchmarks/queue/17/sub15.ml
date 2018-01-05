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
		let enQ = fun (q, e) -> 
			match q with
			| (lst1, lst2) -> (e::lst1, lst2)
		let deQ = fun q ->
			let rec reverse_lst lst =
				match lst with
				| [] -> []
				| hd::tl -> ((reverse_lst tl) @ [hd]) in
			let first_element (lst1, lst2) =
				match lst2 with
				| [] -> raise (EMPTY_Q)
				| hd::tl -> (hd, (lst1, tl)) in
			match q with
			| (lst, []) -> (first_element ([], (reverse_lst lst)))
			| (lst, hd2::tl2) -> (hd2, (lst, tl2))
	end

