exception Error

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
type queue = ((int list) list) * ((int list) list)
exception EMPTY_Q
let emptyQ : queue = ([],[])
let enQ : queue * element -> queue = function ((left_lst, right_lst), element) -> ([element]@left_lst, right_lst) 
let deQ : queue -> (element * queue) = function (left_lst, right_lst) -> match (left_lst, right_lst) with ([],[]) -> (raise EMPTY_Q)
								| (left, []) -> (match (List.rev left) with a::h -> (a, ([], h))
                                                            |[]-> (raise EMPTY_Q))  
								| (left, right) -> ((List.hd right), (left, (List.tl right)) ) 
								
								
end