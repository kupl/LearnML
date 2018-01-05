module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ : queue
    val enQ : queue * element -> queue
    val deQ : queue -> element * queue
  end

module IntListQ : Queue with type element = int list =
   struct
      type element = int list
      type queue = element list * element list
      exception EMPTY_Q
      let emptyQ = (([],[]) : queue)
      let enQ((q:queue),(x:element)) =
            match q with
             (a,b) -> ((x::a, b) : queue)
      let rec deQ((q:queue)) =
            match q with
             (a,b) -> match b with
                          [] -> if a=[] then raise EMPTY_Q
                                else deQ(b, List.rev(a))
                        | l::t -> ((l,(a,t)) : element*queue)
   end;;