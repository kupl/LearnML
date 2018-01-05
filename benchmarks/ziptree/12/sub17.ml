(* Ex9 *)
type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
                 LOC(t, TOP) -> raise(NOMOVE "left of first")
               | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
               | LOC(t, HAND([], up, right)) -> raise(NOMOVE "left of first")
			   
let goRight loc = match loc with
                  LOC(t, TOP) -> raise(NOMOVE "right of last")
				| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
				| LOC(t, HAND(left, up, [])) -> raise(NOMOVE "right of last")
                                   
let goUp loc = match loc with
               LOC(t, TOP) -> raise(NOMOVE "top of first")
             | LOC(t, HAND(l, HAND(ll::left, up, _), r)) -> LOC(ll, HAND(left, up, l @ (t::r)))
			 | LOC(t, HAND(l, HAND(_, up, rr::right), r)) -> LOC(rr, HAND(l @ (t::r), up, right))
			 | LOC(t, HAND([], TOP, r::right)) -> LOC(r, HAND([t], TOP, right))
			 | LOC(t, HAND(l::left, TOP, [])) -> LOC(l, HAND(left, TOP, [t]))

let goDown loc = match loc with
                 LOC(LEAF _, _) -> raise(NOMOVE "bottom of last")
               | LOC(NODE (hd::tl), HAND(l, z, r)) -> LOC(hd, HAND([], HAND(l, z, r), tl))
			   | LOC(NODE (hd::tl), TOP) -> LOC(hd, HAND([], TOP, tl))