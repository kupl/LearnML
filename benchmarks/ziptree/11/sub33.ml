type tree = LEAF of string
	|NODE of tree list

type zipper = TOP
	|HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE

let goLeft loc = match loc with
	LOC(t, TOP) -> raise NOMOVE "left of top"
	|LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|LOC(t, HAND([], up, right)) -> raise NOMOVE "left of first"

let goRight loc =
	match loc with
	LOC(t, TOP) -> raise NOMOVE "right of top"
	|LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t, HAND(left, up, [])) -> raise NOMOVE "right of first"

let goUp loc = 
	match loc with
	LOC (t, TOP) -> raise NOMOVE "up of top"
	|LOC (t, HAND(left, up, right)) -> LOC(NODE((List.rev left)@[t]@right), up)

let goDown loc =
	match loc with
	LOC (LEAF a, zip) -> raise NOMOVE "down of bottom"
	|LOC (NODE lst, zip) ->
		match lst with
		[] -> raise NOMOVE "This is node but not have branches"
		|hd::tl ->  LOC(hd , HAND([], zip, tl))

