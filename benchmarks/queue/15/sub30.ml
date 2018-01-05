(* C:\Users\saigoy\Desktop\queue.ml *)

module type Queue =
  sig
  	type element
  	type queue
  	exception EMPTY_Q
  	val emptyQ : queue
  	val enQ : queue * element -> queue
  	val deQ : queue -> element * queue
  end;;

module IntListQ = 
  struct 
  	type element = int list
  	type queue = ( element list ) * ( element list )
  	exception EMPTY_Q
  	let emptyQ : queue  = ([], [])
  	let enQ (q, e) =
  	match q with
  	| (left, right) -> (e::left , right)
  	
  	let rec deQ q =
  		let reverse lst = 
  			let rec reverse_Aux (lst, result) = 
  			match lst with 
  			| [] -> result
  			| hd::tl -> reverse_Aux(tl, hd::result) in
  		reverse_Aux(lst, []) in
  	match q with
  	| ([] , []) -> raise EMPTY_Q
  	| (left, []) -> deQ ([], (reverse left) )
  	| (left, hd::tl) -> (hd, (left, tl))
  end;;