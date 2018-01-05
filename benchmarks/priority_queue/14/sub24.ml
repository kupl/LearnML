type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap

let rank h = match h with
| EMPTY -> -1
| NODE(r,_,_,_) -> r


let shake (x,l,r) =
  if (rank l) >= (rank r) then
    NODE((rank r)+1, x, l, r)
  else
    NODE((rank l)+1, x, r, l)

let rec merge(h1, h2) = match h1 with
| EMPTY -> h2
| NODE(_,x,_,_) -> 
    (match h2 with
    | EMPTY -> h1
    | NODE(_,y,_,_) ->
        if x > y then
          shake(y,h1,(deleteMin h2))
  else
    shake(x,(deleteMin h1), h2))
  and insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

  and findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x
and deleteMin h = match h with
| EMPTY -> raise EmptyHeap
| NODE(_,x,l,r) -> merge(l,r)
