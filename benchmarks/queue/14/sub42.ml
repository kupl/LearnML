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
    type queue = (element list) * (element list)
    exception EMPTY_Q
    let emptyQ: queue = ([],[])
    let reverse : queue -> queue = 
        fun (inList,outList)-> ([],(List.rev_append inList outList))
    let enQ: queue * element -> queue = 
        fun ((inList,outList), elem) -> (elem::inList,outList)
    let rec deQ: queue -> element * queue = 
        fun (inList,outList) ->
          match outList with
          | hd::tl -> (hd,(inList,tl))
          | _ -> 
             match inList with
             | [] -> raise EMPTY_Q
             | _ -> deQ (reverse (inList,outList))
end
