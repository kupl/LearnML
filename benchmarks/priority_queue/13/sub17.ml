type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int


exception EmptyHeap
let rank = function EMPTY -> -1
		| NODE(r,_,_,_) -> r

let rec merge =
        let shake = function (x, lh, rh) ->
                if(rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
                else NODE(rank lh + 1, x, rh, lh)
        in
        function (EMPTY, b) -> b
                |(a, EMPTY) -> a
                |(NODE(ar,av,alh,arh), NODE(br,bv,blh,brh)) ->
                        if(av<bv) then shake(av, alh, merge(arh, NODE(br,bv,blh,brh)))
                        else shake(bv, blh, merge(brh, NODE(ar,av,alh,arh)))

let insert = function (x, h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
			| NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
			| NODE(_,x,lh,rh) -> merge(lh, rh)

