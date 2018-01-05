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
  type queue = element list list
  exception EMPTY_Q
  let emptyQ = [[];[]]
  let enQ (qu,ele )=  [ele::(List.hd qu); ( List.nth qu 1)]
  let deQ qu =
  	match qu with
		| [[];[]] -> raise EMPTY_Q
  	| [li;[]] -> ((List.hd (List.rev li)) ,[[];(List.tl(List.rev li))] )
		| _ -> (( List.hd (List.nth qu 1) ) , [ (List.hd qu); (List.tl (List.nth qu 1))] ) 
  end	

	
	
	(*module ValidIntListQ = (IntListQ: Queue)*)
	