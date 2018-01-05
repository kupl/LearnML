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
let emptyQ = let l1 = [] in let l2 = [] in
		(l1, l2)
let enQ(q, e) = (e::(fst q), (snd q))
let rec deQ q =	if (snd q) == [] && (fst q) == [] then raise EMPTY_Q
		else if (snd q) == [] then deQ([],List.rev(fst q))	
		else let deque q = match (snd q) with
				|[] -> deQ q
				|hd::tl -> (hd, ((fst q), tl)) in
			deque q
end

