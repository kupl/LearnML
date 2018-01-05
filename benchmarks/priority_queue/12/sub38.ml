type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = match h with 
  | EMPTY -> -1 
  | NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
  if (rank lh) >= (rank rh) 
  then NODE(rank rh+1, x, lh, rh) 
  else NODE(rank lh+1, x, rh, lh) 

let rec merge (h1, h2) = match (h1, h2) with
	| (EMPTY, EMPTY) -> EMPTY
	| (EMPTY, _) -> h2
	| (_, EMPTY) -> h1
	| (NODE (_, x1, lh1, rh1), NODE (_, x2, lh2, rh2)) ->
		if (x1 < x2)
			(*then (shake (x1, h2, (merge (lh1, rh1))))
			else (shake (x2, h1, (merge (lh2, rh2))))*)
		then (shake (x1, lh1, merge (rh1, h2)))
		else (shake (x2, lh2, merge (rh2, h1)))

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh)

(* TEST SET *)
(*
let i1 = EMPTY
let i2 = insert (1, i1)
let i3 = insert (2, i2)
let i4 = insert (3, i3)
let ii2 = insert (11, i1)
let ii3 = insert (12, ii2)
let ii4 = insert (13, ii3)
let m1 = merge (i4, i4)
let m2 = merge (m1, m1)
let mi1 = merge (ii4, ii4)
let mi2 = merge (mi1, mi1)
let mx1 = merge (m2, mi2)
*)