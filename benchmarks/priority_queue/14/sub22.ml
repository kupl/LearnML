type heap = EMPTY
		| NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function
	EMPTY -> -1
	| NODE(r,_,_,_) -> r

let rec merge (lh, rh) =
	let shake = function (x, lh, rh) ->
		if((rank lh) >= (rank rh)) then
			NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)
	in
	let valu h =
		match h with
		| NODE (_, v, _, _) -> v
	in
	let left h =
		match h with
		| NODE (_, _, l, _) -> l
	in
	let right h =
		match h with
		| NODE (_, _, _, r) -> r
	in

	match (lh, rh) with
	| (EMPTY, _) -> rh
	| (_, EMPTY) -> lh
	| _ -> if((valu lh) <= (valu rh)) then
				shake((valu lh), (left lh), merge ((right lh), rh))
			else (* rh is bigger *)
				shake((valu rh), (left rh), merge ((right rh), lh))


let insert = function (x, h)
	-> merge(h, NODE(0, x, EMPTY, EMPTY))
let findMin = function
	EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x
let deleteMin = function
	EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh, rh)


	
