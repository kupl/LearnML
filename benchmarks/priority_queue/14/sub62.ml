type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
| NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
  if(rank lh) >= (rank rh)
	  then NODE(rank rh + 1, x, lh, rh)
	  else NODE(rank lh + 1, x, rh, lh)

let rec merge (lh,rh) =
  match rh with
  | EMPTY -> lh
  | NODE(_,rx,rl,rr) -> 
  begin
    match lh with
    | EMPTY -> rh
    | NODE(_,lx,ll,lr) -> if lx <= rx
    then shake(lx,rh,merge(ll,lr))
    else shake(rx,lh,merge(rl,rr))
  end


let insert = function (x,h) -> merge(h,NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,_,_) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh,rh) -> merge(lh,rh)


