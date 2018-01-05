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
  type queue = Node of element list * element list
  exception EMPTY_Q
  let emptyQ = Node([],[])
  let enQ : queue * element -> queue = function(a,b) ->
      match a with
      | Node([],x) -> Node([b],x)
      | Node(h1::t1,x) -> Node(b::h1::t1,x)
            
  let deQ : queue -> element * queue = function a ->
      match a with
      | Node([],[]) -> raise EMPTY_Q
      | Node([x],[]) -> (x, Node([],[]))
      | Node(x,[]) -> let list_rev = List.rev x in
                            (List.hd list_rev, Node([],List.tl list_rev))
      | Node(x,h2::t2) -> (h2, Node(x,t2)) 
  end



