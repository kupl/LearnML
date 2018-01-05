type item = string
and tree = LEAF of item | NODE of tree list
and zipper = TOP | HAND of tree list * zipper * tree list
and location = LOC of tree * zipper
exception NOMOVE of string
exception TODO of string

let goLeft loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(List.append (List.rev left) (t::right)), up)

let goDown loc = 
	match loc with
	| LOC(LEAF l, _) -> raise (NOMOVE "bottom")
	| LOC(NODE(left::l), up) -> LOC(left, HAND([], up, l))
	| LOC(NODE([]), _) -> raise (NOMOVE "empty node")

let a = "a"
and b = "b"
and c = "c"
and d = "d"
let ab = NODE([LEAF a; LEAF "*"; LEAF b])
and cd = NODE([LEAF c; LEAF "*"; LEAF d])
let test = NODE([ab; LEAF "+"; cd])
