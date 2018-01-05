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
	let emptyQ = ([], [])
	let enQ = fun ((left, right), e) -> (e::left, right)
	let deQ = fun (left, right) -> match right with
										| [] -> (match List.rev left with 
										        | [] -> raise EMPTY_Q
										        | h::t -> (h, ([], t)))
										| h::t -> (h, (left, t))
												
  end
