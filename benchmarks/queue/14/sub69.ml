module type Queue =
sig
type element
type queue
exception EMPTY_Q
val emptyQ: queue
val enQ: queue * element -> queue
val deQ: queue -> element * queue
end

module IntListQ=
struct
type element = int list
type queue = int list * int list
exception EMPTY_Q
let emptyQ = ([],[])
let enQ(queue,node)=
	match queue with
	| (a,b) -> (List.append node a,b)
let deQ(queue)=
	match queue with
	| (a,b) ->
		if b = [] then
			begin
			if a=[] then
				raise (EMPTY_Q)
			else 
			([List.hd (List.rev a)],([],List.tl (List.rev a)))
			end
		else
			([List.hd b],(a, List.tl b))
end
