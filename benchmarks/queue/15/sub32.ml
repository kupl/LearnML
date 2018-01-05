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
    let emptyQ = ([],[])

    let enQ : queue * element -> queue = fun (q,e) ->
      match q with 
      | (e1,e2) -> (e::e1,e2)
    ;;
    let deQ : queue -> element * queue = fun q ->
      match q with
      | (e1,[]) -> (
          let reve1=(List.rev e1)
          in
          match reve1 with
          | [] -> raise EMPTY_Q
          | hd::tl -> (hd,([],tl))
          )
      | (e1,hd::e2) -> (hd, (e1,e2))
    ;;
  end






