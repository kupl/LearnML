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
   type queue = int list list * int list list
   exception EMPTY_Q
   let emptyQ =
     let q : queue = ([],[]) in q

   let enQ : queue * element -> queue = fun ((list_a,list_b),e) -> (e::list_a, list_b)

   let deQ : queue -> element * queue = fun que ->
     match que with
       ([],[]) -> raise (EMPTY_Q)
     | ( q ,[]) -> 
       (match List.rev q with
         | h::t -> ( h, ([], t)) )
     | (list_a, h::list_b) -> (h, (list_a,list_b))

  end