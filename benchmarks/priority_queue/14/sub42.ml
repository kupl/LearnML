type rank = int
type value = int
type heap = EMPTY | NODE of rank * value * heap * heap

exception EmptyHeap

let rank = function EMPTY -> -1
                  | NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let rec merge hp = 
  match (fst hp), (snd hp) with
 | EMPTY, _ -> (snd hp)
 | _, EMPTY -> (fst hp)
 | NODE (_, x1, l1, r1), NODE (_, x2, l2, r2) ->
   if x1 <= x2 then (shake (x1, l1, (merge (r1, (snd hp)))))
   else (shake (x2, l2, (merge ((fst hp), r2))))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)
