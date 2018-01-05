(* HW 2-4 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap 

let rank h = match h with 
  | EMPTY -> -1 
  | NODE(r,_,_,_) -> r 

(* 두 힙을 하나의 노드로 묶어주는 함수
   왼쏠힙 조건에 맞게 배치하고 rank를 1 증가시켜 NODE를 형성한다. *)
let shake (x,lh,rh) = 
  if (rank lh) >= (rank rh) 
  then NODE(rank rh+1, x, lh, rh) 
  else NODE(rank lh+1, x, rh, lh) 

let rec merge (h1, h2) =
	match h1 with 
		| EMPTY -> ( match h2 with
				| EMPTY -> EMPTY
				| NODE (_,_,_,_) -> h2 )
		| NODE (r1,x1,lh1,rh1) -> ( match h2 with
					| EMPTY -> h1
					| NODE (r2,x2,lh2,rh2) -> 
						( if (x1 < x2) then
							shake (x1, (merge (lh1, rh1)), h2)
					          else
							shake (x2, h1, (merge (lh2, rh2))) ))

	
	
	



(* 힙이니까 가장 위에 있는 원소가 minimum이다. *)
let findMin h = match h with 
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 

