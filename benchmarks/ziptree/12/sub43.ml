exception NOMOVE of string
type item = string
type tree = LEAF of item | NODE of tree list
type zipper  = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
let goLeft loc = match loc with
        |LOC(t, TOP) -> raise (NOMOVE "LEFT of top")
        |LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
        |LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
let goRight loc = match loc with
        |LOC(t, TOP) -> raise (NOMOVE "RIGHT of top")
        |LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
        |LOC(t, HAND(left, right, [])) -> raise (NOMOVE "right of last")
let goUp loc = match loc with
        |LOC(t, TOP) -> raise (NOMOVE "up of top")
        |LOC(t, HAND(left, up, right)) -> LOC(NODE(List.rev_append left (t::right)), up)
let goDown loc = match loc with
        |LOC(LEAF a, b) -> raise (NOMOVE "down of last")
        |LOC(NODE [], a) -> raise (NOMOVE "down of empty node")
        |LOC(NODE (h::t), z) -> LOC(h, HAND([], z, t))