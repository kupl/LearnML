type item=string
exception NOMOVE of string 

type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goRight loc=match loc with
LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc=match loc with
LOC(t, TOP) -> raise(NOMOVE "up of top")
| LOC(t, HAND(left, z, right)) -> LOC(NODE (List.append(List.append left [t]) right),z)

let goDown loc=match loc with
LOC(LEAF l,z)->raise (NOMOVE "down of bottom")
|LOC(NODE [], z) -> raise (NOMOVE "down of bottom")
| LOC(NODE(t::right), z) -> LOC(t, HAND([], z,right))
