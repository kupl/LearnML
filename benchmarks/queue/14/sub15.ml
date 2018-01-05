(* 프로그래밍언어 HW2 Exercise 5
   2009-11657 김동현 *)
	
(* 참고: queue with 2 stacks, HOROWITZ&SAHNI, Fundamentals of Data Structures in C, pp. 120~126 *)	
			
module type Queue =
	sig
		type element 
    type queue 
    exception EMPTY_Q 
    val emptyQ: queue 
    val enQ: queue * element -> queue 
    val deQ: queue -> element * queue
	end 
  
module IntListQ =
	struct
		type element = int list 
    (* 큐에 삽입하는 원소의 타입은 'int list' *)
		
	  type queue = Queue of int list list * int list list
		(* 큐를 두 개의 리스트로 구현 *) 
   
	  exception EMPTY_Q 
    (* 빈 큐에서 deQ 연산시 예외 처리 *)
	   
	  let emptyQ = Queue ([], [])
		(* 비어있는 큐. 비어있는 리스트의 쌍 *)
		
	  let enQ (q, elem) =
			match q with
	    | Queue (l, r) -> Queue ([elem]@l, r)
		(* enqueue 연산. 리스트 L에 삽입 *)  
 		
		(* dequeue 연산 *)
    let rec deQ q =
			match q with
			| Queue ([], []) -> raise EMPTY_Q
			(* 빈 큐에서 dequeue 연산 시도시 EMPTY_Q 예외 *) 
      | Queue (l, []) -> deQ (Queue ([], (List.rev l)))
			(* 원소를 빼고 L 리스트를 뒤집어서 R로 *) 
	    | Queue (l, (rh::rt)) -> (rh, Queue (l, rt))
  end 

(* module ValidIntListQ = (IntListQ: Queue) *)