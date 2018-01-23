type tree = LEAF of char
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goRight loc=match loc with
LOC(t, TOP) -> raise (Invalid_argument "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(([t]), up, right))
| LOC(t, HAND(left,up,[])) -> raise (Invalid_argument "right of first");; 

let goUp loc=match loc with
LOC(t, TOP) -> raise(Invalid_argument "up of top")
| LOC(t, HAND(left, z, right)) -> LOC(NODE [(NODE left);t;(NODE right)],z)

let goDown loc=match loc with
LOC(NODE [], z) -> raise (Invalid_argument "down of bottom")
| LOC(NODE (t::right), z) -> LOC(t, HAND([], z,right))
