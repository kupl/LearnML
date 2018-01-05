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
type queue = element list * element list
exception EMPTY_Q
let emptyQ = ([], [])
let enQ ((q1:queue), (q2:element))=
	if (q1=emptyQ) then ([q2], [])
	else (q2::(fst q1), (snd q1))
let deQ (q1:queue)= 
	if((snd q1)=[]) then (List.hd(List.rev(fst q1)), ([],List.tl(List.rev(fst q1))))
	else (List.hd(snd q1), ((fst q1), List.tl(snd q1))) 
end
