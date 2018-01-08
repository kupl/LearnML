type item = string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = 
	match loc with
		LOC(t, TOP) -> raise (NOMOVE "left of top")
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = 
	match loc with
		LOC(l,TOP) -> raise (NOMOVE "right of top")
		|LOC(l, HAND(left, up, t::right)) -> LOC(t, HAND(l::left, up, right))
		|LOC(l, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = 
	match loc with
		LOC(t,TOP) -> raise (NOMOVE "up of top")
		|LOC(t, HAND(left, up, right)) -> LOC(NODE(List.append (List.rev left) (t::right)), up)

let goDown loc =
	match loc with
		LOC(NODE [], _) -> raise (NOMOVE "I don't have siblings")
		|LOC(LEAF d, _) -> raise (NOMOVE "I can't go down")
		|LOC(NODE(h::t), HAND(left, up, right)) -> LOC(h, HAND([], HAND(left, up, right),t))
		|LOC(NODE(h::t), TOP) -> LOC(h, HAND([], TOP, t))