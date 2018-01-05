type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1 
  | NODE (r, _, _, _) -> r 
let findMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x
let getlh = function EMPTY -> raise EmptyHeap 
  | NODE (_, _, lh, _) -> lh 	
 
let rec mergeing = fun (heap1, heap2,list) -> 
	match (heap1, heap2) with
    | (EMPTY, _) -> 
			(heap2,list)
		| (_, EMPTY) ->
			(heap1,list)
		| (NODE(r1,v1,lh1,rh1),NODE(r2, v2,lh2,rh2)) ->
			if (v1 <= v2) then mergeing(rh1,heap2,List.append [NODE(r1,v1,lh1,rh1)] list)
			else mergeing(rh2,heap1,List.append [NODE(r2, v2,lh2,rh2)] list)

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)		
	
let rec shaking(heap, list)=
	match (heap, list) with
    | (_, []) -> 
			heap
		| (_, _) ->
			let node = List.hd(list) in
		shaking(shake(findMin(node),getlh(node),heap),List.tl(list))		

let merge(heap1, heap2) =
	shaking(mergeing(heap1, heap2,[]))
	
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)






	
 
		

	

