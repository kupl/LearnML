(* C:\OCaml\lib\Queue_equal_2stacks.ml *)

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
  	type queue = QUEUE of element list * element list
  	exception EMPTY_Q

  	let emptyQ = QUEUE ([], [])

  	let enQ (qu, elm) = 
	  match qu with
	  |QUEUE (a, b) -> QUEUE (elm::a, b)

	let rec queue_sort qu = 
	  match qu with
	  |QUEUE ([], b) -> QUEUE ([], b)
	  |QUEUE (head::tail, b) -> queue_sort(QUEUE (tail, head::b))
		  
  	let rec deQ qu = 
	  match qu with
	  |QUEUE ([], []) -> raise EMPTY_Q
	  |QUEUE (a, []) -> deQ(queue_sort qu)
	  |QUEUE (a, head::tail) -> (head, QUEUE (a, tail))
  end
