exception NOMOVE of string

type item = string
type tree = LEAF of item
		  | NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")
  | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))

let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "top of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE ((List.rev left)@[t]@(right)), up)

let goDown loc = match loc with
	LOC(LEAF i, _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE [], _) -> raise (NOMOVE "down of empty")
  | LOC(NODE (t::l), z) -> LOC(t, HAND([],z,l))

let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; 
                      LEAF "+"; 
                      NODE [LEAF "c"; LEAF "*"; LEAF "d"]], 
                TOP) 
