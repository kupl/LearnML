type item = string
type tree = LEAF of item
					| NODE of tree list

type zipper = TOP
						| HAND of tree list * zipper * tree list 

type location = LOC of tree * zipper

(*fuck direction*)

let goRight loc = 
  match loc with 
	| LOC(t, TOP) -> raise (Failure "right of top")
  | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (Failure "right of first")

let goUp loc =
	match loc with 
	| LOC(t, TOP) -> raise (Failure "up of top")
	| LOC(t, HAND(left, HAND(left', up, hd::tl), right)) -> LOC(hd, HAND(left', up, tl))

let goDown loc = 
	match loc with 
	| LOC(t, HAND([], up, [])) -> raise (Failure "down of bottom")
  | LOC(t, HAND(left, HAND(left', down, right'), hd::tl)) -> LOC(hd, HAND(left', down, tl))	



