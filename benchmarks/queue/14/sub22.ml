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
    type queue = QUEUE of element list * element list
    exception EMPTY_Q
    let emptyQ = QUEUE ([], [])
    let enQ = function
      | (QUEUE (in_list, out_list), element) ->
         (QUEUE (element::in_list, out_list))
    ;;
    let rec deQ = function
      | QUEUE ([], []) ->
         raise EMPTY_Q
      | QUEUE (in_list, []) ->
         deQ (QUEUE ([], List.rev in_list))
      | QUEUE (in_list, element::out_list) ->
         (element, QUEUE (in_list, out_list))
    ;;
  end
