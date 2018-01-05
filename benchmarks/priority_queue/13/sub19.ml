(* heap is MinHeap *)
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap
let rank = function EMPTY -> -1 | NODE(r,_,_,_) -> r
		
let rec merge : heap * heap -> heap = 
	let shake : value * heap * heap -> heap  = 
		fun (x, lh, rh) ->
			if (rank lh) >= (rank rh)
				then NODE((rank rh) + 1, x, lh, rh)
				else NODE((rank lh) + 1, x, rh, lh)
	in
	fun(h1, h2) -> match h1 with
		| NODE(r1, v1, lh1, rh1) -> ( match h2 with
			| NODE(r2, v2, lh2, rh2) ->
				if v1 < v2 then
					shake(v1, lh1, merge(rh1, h2))
				else
					shake(v2, lh2, merge(rh2, h1))
			| EMPTY -> h1 )
		| EMPTY -> h2
let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
| NODE(_, x, _, _) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_, x, lh, rh) -> merge (lh, rh)
