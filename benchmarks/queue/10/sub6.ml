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
    type queue = Node of element * element
    exception EMPTY_Q
    let emptyQ = Node([], [])
    let enQ (queue, item) = 
    	match queue with
	| Node(left, right) -> Node(List.append item left, right)
    let rec deQ queue =
    	match queue with
	| Node(left, []) -> deQ (Node([], List.rev left))
	| Node(left, right) -> ([List.hd right], Node(left, List.tl right)) 
  end

