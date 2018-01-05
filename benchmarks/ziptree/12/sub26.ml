type item = string
type tree = LEAF of item
	| NODE of tree list

type zipper = TOP
	| HAND of tree list * zipper * tree list



type location = LOC of tree * zipper

exception ERR_LEFT_OF_TOP
exception ERR_LEFT_OF_FIRST
exception ERR_RIGHT_OF_TOP
exception ERR_RIGHT_OF_LAST
exception ERR_DOWN_OF_LEAF
exception ERR_INVALID_NUM_OF_STEM
exception NOMOVE of string 


let goLeft loc = match loc with
		LOC(t, TOP) -> raise ERR_LEFT_OF_TOP
	|	LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|	LOC(t, HAND([], up, right)) -> raise ERR_LEFT_OF_FIRST

let goRight loc = match loc with
		LOC(t, TOP) -> raise ERR_RIGHT_OF_TOP
	|	LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	|	LOC(t, HAND(left, up, []))	-> raise ERR_RIGHT_OF_LAST

let goDown loc = match loc with
	 LOC(pos, whatever) -> (match pos with 
					LEAF _ -> raise ERR_DOWN_OF_LEAF
				|	NODE trList -> LOC ((List.hd trList), HAND([], whatever, List.tl trList))
				)

let goUp loc = match loc with
		LOC(_, TOP) -> raise (NOMOVE "ERR_UP_OF_TOP")
	(*|	LOC(pos, HAND(_, TOP, _)) -> raise (NOMOVE "ERR_UP_OF_TOP")*)
	|	LOC(pos, HAND(left, up, right)) -> LOC(NODE (List.rev_append left (List.append [pos] right)), up)

