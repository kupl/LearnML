type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap

let rank = function EMPTY -> -1
	 | NODE(r, _, _, _) -> r

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)

let rec merge = function (h1,h2) ->
	match h1 with
		| EMPTY -> h2
		| NODE(rank1, value1, lh1, rh1) ->
			match h2 with
			| EMPTY -> h1
			| NODE (rank2, value2, lh2, rh2)->
				if rank1<rank2 then merge(h2,h1) 
				else if lh2=EMPTY &&rh2=EMPTY 
					then if value1 > value2 then shake(value2,lh1,
						merge(rh1, NODE(0,value1,EMPTY,EMPTY)))
					else shake(value1,lh1, merge(rh1, 
						NODE(0, value2, EMPTY, EMPTY)))
				else if (rank h1)>(rank h2)+1
					then if value1 > value2 then merge(merge(shake(value2, lh1,
						merge(rh1, NODE(0,value1, EMPTY, EMPTY))),lh2),rh2)
					else merge(merge(shake(value1, lh1, merge(rh1, NODE(0,
						value2, EMPTY, EMPTY))),lh2),rh2)
				else let aftermove = if value1 > value2 then merge(lh2,rh2) else merge(lh1,rh1) in
					if value1 > value2 then if (rank h1) > (rank aftermove)+1
						then merge(merge(lh1,rh1),shake(value2,lh2,
							merge(rh2,NODE(0,value1,EMPTY,EMPTY))))
						else shake(value2, h1, aftermove)
					else if (rank h2) > (rank aftermove)+1
						then merge(shake(value1, lh1, merge(rh1, 
							NODE(0,value1,EMPTY,EMPTY))),merge(lh2,rh2))
						else shake(value1, aftermove, h2)
	

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
	    | NODE( _,x, _, _) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
	    | NODE( _,x,lh,rh) -> merge(lh,rh)
