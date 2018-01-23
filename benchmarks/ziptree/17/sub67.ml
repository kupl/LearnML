exception NOMOVE of string

type item = string
type tree = 
    |LEAF of item
    |NODE of tree list

type zipper = 
    |TOP
    |HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goRight loc = 
    match loc with
    |LOC(t, TOP) -> raise (NOMOVE "right of top")
    |LOC(t, HAND(l, up, right::right)) -> LOC(l, HAND(t::left, up, right))
    |LOC(t, HAND(left, up, []))-> raise (NOMOVE "right of first")

let goUp loc =
    match loc with
    |LOC(t, z) ->   (match z with
                    |TOP -> raise("top")
                    |HAND(l, u, r) ->   if l = [] then LOC(NODE(t::r), up)
                                        else LOC(NODE((List.rev_append l t)::r), up)
                    )
let goDown loc = 
    match loc with
    |LOC(LEAF l, z) -> raise (NOMOVE "bottom")
    |LOC(NODE [], z) -> raise (NOMOVE "bottom")
    |LOC(NODE (h::t), z) -> LOC(h, HAND([], z, t))
