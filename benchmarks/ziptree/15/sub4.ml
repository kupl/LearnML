exception NOMOVE of string;;

type item = string
and  tree = LEAF of item
	   | NODE of tree list
and  zipper = TOP
            | HAND of tree list * zipper * tree list
and  location = LOC of tree * zipper;;

let goLeft loc = match loc with
      LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first");;

let goRight : location -> location = fun loc ->
match loc with
LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left,up,r::right)) -> LOC(r, HAND(t::left,up,right))
| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first");;

let goUp : location -> location = fun loc ->
match loc with
LOC(t, TOP) -> raise (NOMOVE "top of top")
| LOC(t, HAND([],up,right)) -> LOC( NODE(List.concat [[t];right]), up)
| LOC(t, HAND(lh::left,up,right)) -> LOC( NODE (List.concat [left;[lh;t];right] ), up);;

let goDown : location -> location = fun loc ->
match loc with
LOC(LEAF _, zip) -> raise (NOMOVE "down of down")
| LOC(NODE [], zip) -> raise(NOMOVE "down of down")
| LOC(NODE(t::tl),zip) -> LOC(t, HAND([], zip,tl));;
