
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
		| NODE (r,_,_,_) -> r


let rec findMin = function EMPTY -> raise EmptyHeap
			| NODE (_, x, _, _) -> x
and deletMin = function EMPTY -> raise EmptyHeap
			| NODE (_, x, lh, rh) -> merge (lh, rh)
and insert = function (x,h) -> merge (h, NODE(0, x, EMPTY, EMPTY))

and shake = function (x, lh, rh) ->
 if (rank lh) >= (rank rh)
  then NODE (rank rh + 1, x, lh, rh)
  else NODE (rank lh +1, x, rh, lh)


and merge : heap * heap -> heap = fun (lh, rh) ->
match (lh,rh) with
|(EMPTY, EMPTY) -> EMPTY
| (EMPTY, rh) -> rh
| (lh, EMPTY) -> lh
| ( NODE (lr, lx, llh, lrh), NODE (rr, rx, rlh, rrh) ) ->
let out = 
	if (lx<rx) then (
		let temp = merge (lrh, NODE (rr, rx, rlh, rrh))in 
		shake (lx, llh, temp)
	) else (
		let temp = merge (rrh, NODE (lr, lx, llh, lrh)) in
		shake (rx, rlh, temp)
	) 
in out



