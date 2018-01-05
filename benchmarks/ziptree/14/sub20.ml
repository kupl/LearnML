(*********************************
 ** PL::HW[02].Problem[02]      **
 **                             **
 ** Mod. Init: 2014-09-27 11:09 **
 ** Mod. Fin.: 2014-09-27 12:38 **
 **                             **
 ** Writ. by : CMS              **
 *********************************)

type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(left@[t]@right), up)

let goDown loc = match loc with
	  LOC(LEAF leaf, here) -> raise (NOMOVE "down of leaf")
	| LOC(NODE tree_list, here) -> 
	(
		match tree_list with
		| [] -> raise (NOMOVE "down of empty node; illegal")
		| t::right -> LOC (t, HAND([], here, right)); (* go to the leftest element of tree list *)
	)

