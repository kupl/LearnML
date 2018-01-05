module type Queue = 
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end
module IntListQ:Queue = 
    struct
      type element = int list
      type queue = element list * element list
      exception EMPTY_Q
      let emptyQ = ([],[])
      let enQ (q,e) =
           let (a, b) = q in
                if b = [] then ([],(List.rev a)@[e])
                else ([e]@a,b)
      
      let deQ q = 
           if(q = emptyQ) then raise EMPTY_Q
           else let (a,b) = q in
                        if (b = []) then 
                                let a = (List.rev a) in (List.hd a,([],List.tl a))
                        else (List.hd b,(a,List.tl b))

    end
        
