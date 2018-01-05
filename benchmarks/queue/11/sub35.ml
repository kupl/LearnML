module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end
module IntListQ : Queue with type element = int list = 
  struct 
    type element = int list
    type queue = element*element
    exception EMPTY_Q
    let emptyQ=([],[])
    let enQ (q,e)=(match q with (e1,e2)->(e@e1,e2))
    let deQ q=(match q with (e1,e2)->
            if e2!=[] then ([List.hd(e2)],(e1,List.tl(e2)))
            else if e1!=[] then ([List.hd(List.rev(e1))],([],List.tl(List.rev(e1))))
            else raise (EMPTY_Q)
            )
  end
