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
    type queue = element list * element list
    exception EMPTY_Q

    let emptyQ: queue = ([],[])
    let enQ ((q:queue), (e:element)) = 
      match q with
      |(lq, rq)->(e::lq, rq)

    let rec deQ (q:queue):element*queue = 
      match q with
      |(lq,[]) -> deQ(([],List.rev lq))
      |(lq,(h::t))-> (h,(lq, t))
  end

