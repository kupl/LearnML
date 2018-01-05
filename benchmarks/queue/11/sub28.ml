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
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([], [])

	let enQ(q, ele) = 
		let (a,b) = q in
			(ele::a,b)

	let deQ(q) = 
		let (a,b) = q in
			match b with 
			| [] -> (
				let rev = List.rev a in
				match rev with 
				[] -> raise EMPTY_Q
				| hd::ta -> (hd, ([], ta))
			)
			| hd::ta -> (hd, (a,ta))
end
