exception NOMOVE of string
type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
			LOC(t,TOP) -> raise (NOMOVE "left of top")
			| LOC (t, HAND (l :: left, up, right)) -> LOC (l, HAND(left, up, t::right))
			| LOC (t, HAND ([],up,right)) -> raise (NOMOVE "left of first")
let pp = LOC (LEAF "*",HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],TOP,[]), [LEAF "d"]));;
let goRight loc = match loc with
			LOC(t,TOP) -> raise (NOMOVE "right of top")
			| LOC (t, HAND (left, up, r :: right)) -> LOC (r, HAND(t :: left, up, right))
			| LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of first")
let goUp loc = match loc with
			LOC(t,TOP) -> raise (NOMOVE "up of top")
			| LOC(t, HAND (left, cen, right)) -> LOC(NODE ((List.rev left) @ [t] @ right), cen)
let goDown loc = match loc with
			LOC(LEAF le, some) -> raise (NOMOVE "can't go down since it is a leaf")
			| LOC (NODE [], some) -> raise (NOMOVE "can't go down since it is null node")
			| LOC (NODE (h::t), some) -> LOC (h, HAND([], some, t))
