type item = string
type tree = LEAF of item
		  | NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft loc = match loc with
               | LOC(t, TOP) -> raise (NOMOVE "left of top")
               | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
               | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
                | LOC(t, TOP) -> raise (NOMOVE "right of top")
                | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
                | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = match loc with
       | LOC(t, TOP) -> raise (NOMOVE "top of top")
       | LOC(t, HAND(left, x, right)) -> LOC(NODE ((List.rev left)@t::right), x)

let goDown loc = match loc with
      | LOC(LEAF f, _) -> raise (NOMOVE "bottom of bottom")
      | LOC(NODE [], _) -> raise (NOMOVE "empty")
      | LOC(NODE t, HAND(left, up, right)) -> LOC(List.hd t, HAND([], HAND(left, up, right), List.tl t))
      | LOC(NODE t, TOP) -> LOC(List.hd t, HAND([], TOP, List.tl t))