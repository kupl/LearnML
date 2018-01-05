module type Queue =
  sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
  end



module IntListQ =
  struct
	type element = int list
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (qu, lst) = 
		match qu with
		| (lst1,lst2) -> ((lst :: lst1),lst2)
	let deQ qu = 
		match qu with
		| ([],[]) -> raise EMPTY_Q
		| ([], lst2) -> ((List.hd lst2), ([], List.tl lst2))
		| (lst1,[]) -> ((List.nth lst1 (List.length lst1 - 1)),([], (List.tl (List.rev lst1))))
		| (lst1,hd::tl) -> if tl = [] then (hd, (lst1,[]))
			           else (hd,(lst1,tl))
		
  end	

 
  


