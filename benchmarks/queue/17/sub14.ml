module type queue = 
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
    type queue = int list list * int list list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (q,a) = (a::fst q, snd q)    
    let deQ q = 
      let rec addright (lst, a) = match lst with
        |[] -> a::[]
        |b::lst2 -> b::(addright(lst2, a))
      in
      let rec reverse lst = match lst with
        |[] -> []
        |hd::tl -> addright(reverse(tl), hd)
      in
      match q with
      |([],[]) -> raise EMPTY_Q
      |(f, []) ->
        let rf = reverse f in
        (List.hd rf,([],List.tl rf)) 
      |(f, a::s) -> (a, (f,s))
  end

