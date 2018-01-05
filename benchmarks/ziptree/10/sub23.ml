exception NOMOVE 

type tree = LEAF of item | NODE of tree list
and item = string
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper  

let goLeft loc = match loc with
		LOC (t, TOP) -> raise NOMOVE
		| LOC (t, HAND(l::left, up, right)) -> LOC(l, HAND (left,up,t::right))
		| LOC (t, HAND([],up,right)) -> raise NOMOVE  

let goRight loc = match loc with
		LOC (t, TOP) -> raise NOMOVE
		| LOC (t, HAND(left, up, l::right)) -> LOC (l, HAND (t::left, up, right))
		| LOC (t, HAND(left,up,[])) -> raise NOMOVE

let goDown loc = match loc with
		LOC (NODE [], TOP) -> raise NOMOVE
		| LOC (NODE [], HAND (left, up, right)) -> raise NOMOVE
		| LOC (NODE (h::t), TOP) -> LOC (h, HAND ([], TOP, t))
		| LOC (NODE (h::t), HAND (left, up, right)) -> LOC (h, HAND([], HAND (left, up, right), t))   
		| LOC (LEAF b, TOP) -> raise NOMOVE
		| LOC (LEAF b, HAND (l,u,r)) -> raise NOMOVE

let goUp loc = match loc with
		LOC (NODE [], TOP) -> raise NOMOVE
		| LOC (NODE [], HAND (left, up, right)) -> raise NOMOVE
		| LOC (NODE (h::t), TOP) -> raise NOMOVE
		| LOC (NODE l, HAND (left, up, right)) -> LOC (NODE(List.append left ((NODE l)::right)), up)
		| LOC (LEAF b, TOP) -> raise NOMOVE
		| LOC (LEAF b, HAND (left, up, right)) -> LOC (NODE (List.append left ((LEAF b)::right)), up)
