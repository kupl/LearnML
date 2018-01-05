
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
type element =int list
type queue=element list*element list
exception EMPTY_Q
let emptyQ=([],[])
let rec move qu=
	match qu with
	|(a,b)->
	(
		match a with
		|[]->qu
		|hd::tl->move (tl,hd::b)
	)
let enQ t =
	match t with
	|(qu, inl)->
	(
		match qu with
		|(ele,ele2)->(inl::ele,ele2)
	)
let rec deQ qu=
	match qu with
	|(ele,ele2)->
		match ele2 with
		|[]->if ele=[] then raise EMPTY_Q
			else deQ(move qu)
		|hd::tl-> (hd,(ele,tl))
		
end


