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
    type queue = element * element
    exception EMPTY_Q
    let emptyQ = ([]:element),([]:element)
    let enQ ((q:queue), (elem:element)) = 
      match q with
      | (lq,rq) -> ((List.append elem lq, rq):queue)

    let deQ (q:queue) =
      match q with
      | ([],[]) -> raise EMPTY_Q
      | (lq,[]) -> let rq = (List.rev lq) in 
      ([(List.hd rq)]:element), (([],(List.tl rq)):queue)
      | (lq,hd::[]) -> ([hd]:element), (([], List.rev lq):queue)
      | (lq,hd::tl) -> [hd], (lq,tl)

  end

    let myQ = IntListQ.emptyQ
    let yourQ = IntListQ.enQ(myQ, [1])
    let (x,restQ) = IntListQ.deQ yourQ
    let hisQ = IntListQ.enQ(myQ, [2])

