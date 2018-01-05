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
	type queue = element * element
	exception EMPTY_Q
	let emptyQ : queue = ( [], [] )
	
	let enQ : queue * element -> queue = 
	(
		function ( q, itm ) -> ( itm @ (fst q), snd q )
	);;
	
	let deQ : queue -> element * queue =
	(
		function q ->
		(
			if 0 != ( List.length(snd q) )
			then ( [ List.hd(snd q) ], (fst q, List.tl(snd q)) )
			else
				if 0 != ( List.length(fst q) )
				then ( [ List.hd( List.rev(fst q) ) ], ( [],  List.tl( List.rev(fst q) ) ) )
				else raise EMPTY_Q
		)
	);;
end