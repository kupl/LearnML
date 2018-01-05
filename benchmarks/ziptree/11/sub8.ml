type tree = LEAF of item
        | NODE of tree list
and item = string
exception NOMOVE of string

type zipper = TOP
        | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goRight loc = match loc with
        LOC(t, TOP) -> raise (NOMOVE "right of top")
        | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
        | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of tree")

let goUp loc = match loc with
        LOC(t, TOP) -> raise (NOMOVE "top")
        | LOC(NODE t, HAND(left, up, right)) -> let n = (List.rev (t@left))@right in LOC(NODE n,up)
        | _ -> raise (NOMOVE "error")

let goDown loc = match loc with
        LOC(LEAF _, _) -> raise (NOMOVE "bottom")
        | LOC(NODE (hd::t), g) -> LOC(hd, HAND([], g, t))
        | _ -> raise (NOMOVE "error")
