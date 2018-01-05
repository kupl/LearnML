exception NOMOVE of string
type item = string

type tree = LEAF of item
|NODE of tree list

type zipper = TOP
|HAND of tree list * zipper * tree list

type location = LOC of tree* zipper

let  goLeft loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "left of the top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of the first")

let goRight loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "right of the top")
| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of the last")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))


let rec  tidyTreeList left right = match left with
|[] -> NODE right
|a::b -> tidyTreeList b (a::right)

let goUp loc = match loc with
|LOC(t, TOP) -> raise (NOMOVE "above the top")
|LOC(t, HAND(left, up, right)) -> LOC(tidyTreeList left (t::right), up)

let goDown loc = match loc with
LOC(LEAF _ , _) -> raise (NOMOVE "below of the bottom")
| LOC(NODE [], _) -> raise (NOMOVE "Oops!")
| LOC(NODE(a::b), zip) -> LOC(a, HAND([],zip, b))



