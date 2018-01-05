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
	
	let emptyQ = ([], [])

	let enQ (q, e) = let ql=(fst q) in
					 let qr=(snd q) in
					   ((List.append [e] ql), qr)
					   
	let deQ q = let ql=(fst q) in
				let qr=(snd q) in
				  if (List.length qr)>0 then ((List.hd qr), (ql, (List.tl qr)))
				  else if (List.length ql)>0 then 
					let ql_rev = (List.rev ql) in
					  ((List.hd ql_rev), ([], (List.tl ql_rev)))
				  else raise EMPTY_Q
  end
