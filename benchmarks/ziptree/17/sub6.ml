exception NOMOVE of string

type item = string
type tree = LEAF of item
            | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc =
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight: location -> location = fun loc ->
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp: location -> location = fun loc ->
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "upside of top")
    | LOC(t, HAND(ls, up, rs)) -> LOC(NODE((List.rev ls)@t::rs), up)

let goDown: location -> location = fun loc ->
    match loc with
    | LOC(LEAF(_), _) -> raise (NOMOVE "downside of bottom")
    | LOC(NODE [], _) -> raise (NOMOVE "downside of null")
    | LOC(NODE(hd::tl), up) -> LOC(hd, HAND([], up, tl))

