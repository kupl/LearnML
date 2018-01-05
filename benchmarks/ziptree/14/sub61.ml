type tree =
    | LEAF of item
    | NODE of tree list
and item = string;;

type zipper =
    | TOP
    | HAND of tree list * zipper * tree list;;

type location =
    | LOC of tree * zipper;;

exception NOMOVE of string;;

let goLeft = function
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first");;

let goRight = function
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of last");;

let goUp = function
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
    | LOC(t, HAND(left, up, right)) -> LOC((NODE (List.append left
    (t::right))),up);;

let goDown = function
    | LOC(LEAF _,_) -> raise (NOMOVE "down of leaf")
    | LOC(NODE(first::rest),zipper) -> LOC(first, HAND([], zipper, rest))
    | LOC(NODE [],_) -> raise (NOMOVE "down of node with no child");;
