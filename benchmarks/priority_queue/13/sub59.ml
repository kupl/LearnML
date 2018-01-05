type heap=
    EMPTY
	|NODE of rank*value*heap*heap
and rank=int
and value=int

exception EmptyHeap
let rank = function 
    EMPTY -> -1
    | NODE(r,_,_,_) -> r

let findMin = function 
    EMPTY -> raise EmptyHeap
    | NODE(_,x,_,_) -> x

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)

let rec merge : heap*heap->heap=
   fun (lh,rh)->
       if lh=EMPTY then rh
	   else if rh=EMPTY then lh
	   else if (findMin lh)>(findMin rh) then merge(rh,lh)
	   else 
	       match lh with
		   |EMPTY -> rh
		   |NODE (r,x,lchild,rchild) ->
		       if(lchild=EMPTY) then NODE(r,x,(merge(rchild,rh)),EMPTY)
               else shake(x,lchild,(merge(rchild,rh)))


let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function 
	EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge(lh,rh)
