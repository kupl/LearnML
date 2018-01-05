type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                    | NODE(r,_,_,_) -> r


let shake = function (x,lh,rh) ->
    if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)


let rec merge(h1,h2)=
	let forshake(rank,value,lh,rh)=
	shake(value,lh,rh) in
	match (h1,h2) with
	|(EMPTY,_)->h2
	|(_,EMPTY)->h1
	|(NODE(rank1,val1,lh1,rh1),NODE(rank2,val2,lh2,rh2)) ->
		if val1>val2
			then 
				if rh2=EMPTY
					then shake(val2,lh2,h1)
					else forshake(rank2,val2,lh2,merge(h1,rh2))
			else
				if rh1=EMPTY
					then shake(val1, lh1,h2)
					else forshake(rank1,val1,lh1,merge(h2,rh1))
				
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
					| NODE(_,x,_,_) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
						| NODE(_,x,lh,rh) -> merge(lh,rh)
