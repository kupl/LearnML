exception NOMOVE of string (*done*)

type item = string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft (loc: location): location =
	 match loc with
	| LOC(t, TOP) -> raise (NOMOVE "just top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "leftist")

let goRight (loc: location): location = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "just top")
	| LOC(t, HAND(left, up, r::right)) ->	LOC(r, HAND (t::left, up, right))
	| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "rightist")

let goUp (loc: location): location = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "just top")
	| LOC(t, HAND(left, up, right))
	 -> LOC (NODE ((List.rev left)@[t]@right), up)		

let goDown (loc: location): location =
	match loc with
	| LOC (LEAF l, _) -> raise (NOMOVE "just leaf")
	| LOC (NODE tlist, zip)
	 -> LOC (List.hd tlist, HAND ([], zip, List.tl tlist))
