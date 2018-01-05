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
      let emptyQ = ([],[])
      let enQ(que, ele) =  match que with
      |(a,b) -> (ele::a, b)
      let deQ que = match que with
      |([],[])-> raise EMPTY_Q
      |(a,[])-> 
          let b::bb = List.rev(a) in
          (b, ([],bb))
      |(a,b::bb) -> (b, (a, bb))

  end

