exception Error of string;;

type item = string;;

type tree = LEAF of item
	    | NODE of tree list;;

type zipper = TOP
	      | HAND of tree list * zipper * tree list;;

type location = LOC of tree * zipper;;

let goLeft loc = match loc with
    LOC(t, TOP) -> raise (Error "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (Error "left of first");;

let goRight loc = match loc with
    LOC(t, TOP) -> raise (Error "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (Error "right of first");;

let goUp loc = match loc with
    LOC(t, TOP) -> raise (Error "Here is top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE(List.append left (t::right)), up);;
				
let goDown loc = match loc with
    LOC((LEAF i), zip) -> raise (Error "Here is leaf")
  | LOC((NODE []), zip) -> raise (Error "Here is leaf")
  | LOC((NODE l), TOP) -> LOC((List.hd l), HAND([], TOP, (List.tl l)))
  | LOC((NODE l), HAND(left, up, right)) -> LOC((List.hd l), HAND(left, HAND(left, up, right), (List.tl l) ));;
