(*2011-11004 ³²À±¼® ¹®Á¦ 5*)

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
let emptyQ = ([],[])
let enQ ((en, de), e)= 
	if List.length en > 10 then 
		let dein = List.rev en in
		let de = List.append de dein in		
		(e::en, de) 
	else
		(e::en, de)
let deQ ((en,de))=  match de with
	[] -> raise EMPTY_Q
	| h::t -> (h, (en,t)) 	
end



