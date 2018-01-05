module L=List
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
	type queue = TLIST of element list*element list
	exception EMPTY_Q
	let emptyQ = TLIST([],[])
	let enQ (queue,element) = match queue with
		|TLIST(hd,tl)->TLIST(element::hd,tl)
	let deQ queue = if queue=emptyQ then raise EMPTY_Q
			else match queue with
		|TLIST(hd,tl)->
			let leng=L.length tl in
			if leng=0 then (L.hd(L.rev(hd)),
				TLIST([],L.tl(L.rev(hd))))
			else (L.hd(tl),TLIST(hd,L.tl(tl)))
end
