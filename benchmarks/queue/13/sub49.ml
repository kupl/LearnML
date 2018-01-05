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
	type element= int list
	type queue= (element list)*(element list)
	exception EMPTY_Q
	let emptyQ= ([], [])
	let enQ (q, ele)=
		match q with (front, back)-> (ele::front, back)
	let deQ q=
		match q with (front, back)->
			if ((List.length back)=0 && (List.length front)=0) then raise EMPTY_Q
			else if (List.length back)=0 then ((List.hd (List.rev front)),(back, (List.tl (List.rev front)))) 
			else ((List.hd back), (front, (List.tl back)))
end


	
