type item = string
type tree = LEAF of item | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string


let goLeft loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))


let goUp loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> 
			LOC((NODE ((t::left)@right)), up)

let goDown loc =
	match loc with
	| LOC(LEAF l, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "no child of the node")
	| LOC(NODE (h::t), zip) ->
			LOC(h, HAND([], zip, t))   


