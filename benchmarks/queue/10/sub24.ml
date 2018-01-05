(*hw2-4 컴퓨터 공학부 2008-11641 신희식*) 

module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end
module IntListQ   =
	struct
		type element = int list
		type queue = 
			(element list) * (element list)
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ ((first_stack,second_stack),element) =
			((element::first_stack),second_stack)
		let rec deQ (first_stack, second_stack) =
			let rec make_queue first second =
				match first with
				[] -> []
				|[a] -> (a::second)
				|(h::t) -> (make_queue t (h::second))
			in
			match (first_stack, second_stack) with
			([],[]) -> (raise EMPTY_Q)
			|(h::t,[]) ->
				(deQ ([],(make_queue first_stack second_stack)))
			|(a,h::t) ->
				(h,(a,t))
	end
