type heap =
  | EMPTY
  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank : heap -> rank =  
  fun h -> match h with 
  | EMPTY -> -1
  | NODE (r,_,_,_) -> r
let shake : value * heap * heap -> heap = 
  fun (v, lh, rh) ->
    if (rank lh) >= (rank rh)
    then NODE (rank rh+1, v, lh, rh)
    else NODE (rank lh+1, v, rh, lh)
let rec merge : heap * heap -> heap = 
  fun (h1, h2) -> match h1, h2 with 
  | EMPTY, _ -> h2
  | _, EMPTY -> h1
  | NODE (r1, v1, lh1, rh1), NODE(r2,v2,lh2,rh2) ->
      if (v1 >= v2) then (shake (v2, h1, merge (lh2, rh2)))
      else (shake (v1, (merge (lh1, rh1)), h2)) 
let insert : value * heap -> heap =
  fun (v, h) -> merge(h, NODE(0,v,EMPTY,EMPTY)) 
let findMin : heap -> value = 
  fun h -> match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,v,_,_) -> v
let deleteMin : heap -> heap = 
  fun h -> match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_,v,lh,rh) -> merge(lh,rh)

