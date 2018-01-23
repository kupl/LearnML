 exception NOMOVE of string

type item = string
type tree = LEAF of item
	| NODE of tree list
type zipper = TOP
	| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

(* LOC(t, TOP) -> t is already including the entire tree *)

let goLeft loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "left of top")
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "right of top")
		| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(r::left, up, right))
		| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "up of top")
		| LOC(t, HAND(left, HAND (tl, z, tr), right)) -> LOC(NODE ((List.rev left)@ [t]@ right), HAND(tl, z, tr))
		| LOC(t, HAND(left, TOP,right)) -> LOC(NODE ((List.rev left)@ [t]@ right), TOP)

let goDown loc = match loc with
		| LOC(LEAF l, _) -> raise (NOMOVE "down of bottom")
		| LOC(NODE [], _) -> raise (NOMOVE "empty in lower")
		| LOC(NODE (hd::tl), TOP) -> LOC(hd, HAND([], TOP, tl))
		| LOC(NODE (hd::tl), HAND(left, up, right)) -> LOC(hd, HAND([], HAND(left, up, right), tl))
