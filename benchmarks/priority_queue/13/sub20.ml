type heap = EMPTY
		| NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
				| NODE (r, _, _, _) -> r


let shake = function (x, lh, rh) ->
				if (rank lh) >= (rank rh)
					then NODE (rank rh + 1, x, lh, rh)
					else NODE (rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
	match (h1, h2) with
	| (_, EMPTY) -> h1
	| (EMPTY, _) -> h2
	| (NODE (r1, v1, lh1, rh1), NODE (r2, v2, lh2, rh2))
			-> if v1>v2 then merge(h2, h1)
			else shake (v1, lh1, merge (rh1, h2))



let insert = function (x, h) -> merge(h, NODE (0, x, EMPTY, EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
					| NODE (_, x, _, _) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
						| NODE (_, x, lh, rh) -> merge (lh, rh)

(*
let t2 = insert (1, insert (10, insert (30,EMPTY))) in 
let t3 = insert (25, insert (34, insert (9, insert (11,EMPTY)))) in 
let t5 = merge (t2,t3) in 
(findMin t5,findMin (deleteMin t5)) (* 1,9 *)
*)
