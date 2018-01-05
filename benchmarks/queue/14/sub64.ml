module type Queue = 
    sig
      type element
      type queue
      exception EMPTY_Q
      val emptyQ: queue
      val enQ: queue * element -> queue
      val deQ: queue -> element * queue
    end;;

module IntListQ = 
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = ([],[])
    let enQ ((l,r), x) = 
      let t = (x::l, r) in
      match t with (ll,rr) -> 
        if List.length ll <= List.length rr then (ll,rr)
        else ([], rr @ List.rev ll)
    let deQ que = 
      match que with
        ([],[]) -> raise EMPTY_Q
      | (x::l, []) -> (x,(l,[]))
      | (l,x::r) -> (x,(l,r))
  end;;