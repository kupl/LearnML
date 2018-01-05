type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank = function EMPTY -> -1
        | NODE (r, _, _, _) -> r

let shake = function (x, lh, rh) ->
        if (rank lh) >= (rank rh) then
                NODE ((rank rh) + 1, x, lh, rh)
        else
                NODE ((rank lh) + 1, x, rh, lh)

let rec merge (lh, rh) =
        match lh with
        | EMPTY -> rh
        | NODE (lrnk, lval, lheap1, lheap2) -> (
                match rh with
                | EMPTY -> lh
                | NODE (rrnk, rval, rheap1, rheap2) -> (
                        if (lval < rval) then
                                shake (lval, lheap1, merge (lheap2, rh))
                        else
                                shake (rval, rheap1, merge (rheap2, lh))
                )
        )

exception EmptyHeap

let insert = function (x, h) -> merge (h, NODE (0, x, EMPTY, EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
	| NODE (_, x, _, _) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE (_, x, lh, rh) -> merge (lh, rh)

