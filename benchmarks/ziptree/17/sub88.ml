type item = string
type tree = LEAF of item
	| NODE of tree list

type zipper = TOP
	| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
	LOC(t,TOP)-> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left,up,right))->LOC(l,HAND(left,up,t::right))
	| LOC(t, HAND([],up, right))->raise (NOMOVE "left of first")

let goRight: location->location =
	fun loc ->
	match loc with
	| LOC(t,TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left,up,r::right))->LOC(r,HAND(t::left,up,right))
	| LOC(t, HAND(left,up,[]))->raise(NOMOVE "right of first")

let goUp: location->location =
	fun loc ->
	match loc with
	| LOC(t,TOP)->raise(NOMOVE "up of top")
	| LOC(t, HAND(left,up,right)) ->
		let rev_left = List.rev left in
		LOC( NODE(rev_left@[t]@right), up) 

let goDown: location->location =
	fun loc ->
	match loc with
	| LOC(LEAF t,zipper)->raise(NOMOVE "down of top")
	| LOC(NODE t,zipper)->
		match t with
		| [] -> raise(NOMOVE "down of top")
		| [a] -> LOC(a, HAND([],zipper,[]))
		| a::rest ->LOC(a,HAND([],zipper,rest))
