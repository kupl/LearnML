(* 프로그래밍언어 HW2 Exercise 2
   2009-11657 김동현 *)

(* merge는 O(logn), 오른쪽 척추에 붙어있는 노드 수는 많아야 └log(n+1)┘ *)
										
(* 참고: HOROWITZ&SAHNI, Fundamentals of Data Structures in C, pp. 445~453 *)	
			
type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 
 
exception EmptyHeap 

let rank = function EMPTY -> -1 
                  | NODE(r,_,_,_) -> r
  
let findMin = function EMPTY -> raise EmptyHeap 
                     | NODE(_,x,_,_) -> x 

let shake = function (x,lh,rh) ->
	     if (rank lh) >= (rank rh)
	      then NODE(rank rh + 1, x, lh, rh) 
        else NODE(rank lh + 1, x, rh, lh) 

let getLeftSubHeap h =
	match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,_,lh,_) -> lh
(* 주어진 힙의 왼쪽 서브힙을 리턴 *)

let getRightSubHeap h =
	match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,_,_,rh) -> rh
(* 주어진 힙의 오른쪽 서브힙을 리턴 *)

let compareHeap (h1, h2) =
	match (h1, h2) with
	| (EMPTY, _) -> raise EmptyHeap
	| (_, EMPTY) -> raise EmptyHeap
	| (h1, h2) -> if (findMin h1) < (findMin h2) then (h1, h2)
		            else (h2, h1)
(* 두 힙의 최소값을 비교하여 (최소값이 더 작은 힙, 최소갑이 더 큰 힙)의 pair 리턴 *)

let rec merge (h1, h2) =
	match (h1, h2) with
	| (h1, EMPTY) -> h1
	| (EMPTY, h2) -> h2
	(* 두 힙 중 한 힙이 빈 힙이면 그냥 나머지 힙을 리턴 *)
	| (h1, h2) -> let (ha, hb) = compareHeap (h1, h2) in
		            shake((findMin ha), (getLeftSubHeap ha), merge(getRightSubHeap ha, hb))
	(* shake 함수를 이용하여 정의 *)
	
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap 
                       | NODE(_,x,lh,rh) -> merge(lh,rh) 
