type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap

let rank h =
	match h with
	EMPTY -> -1
	|NODE(r, _, _, _) -> r

let rec merge (lh, rh) =
	
	let shake (x, lh_, rh_) =
		if((rank lh_) >= (rank rh_))
			then NODE(rank rh_ + 1, x, lh_, rh_)
		else NODE(rank lh_ + 1, x, rh_, lh_)
	in

	match lh with
	EMPTY -> rh
	|NODE(lr, lx, llh, lrh) -> (
		match rh with
		EMPTY -> lh
		|NODE(rr, rx, rlh, rrh) ->
			if(lx < rx) then shake(lx, merge(llh, lrh), rh)
			else shake(rx, lh, merge(rlh, rrh))
		)

let insert (x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin h =
	match h with
	EMPTY -> raise EmptyHeap
	|NODE(_, x, _, _) -> x

let deleteMin h =
	match h with
	EMPTY -> raise EmptyHeap
	|NODE(_, x, lh, rh) -> merge(lh, rh)
