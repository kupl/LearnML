type item = string
type tree = LEAF of item
| NODE of tree list;;

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper;;

exception NOMOVE of string

let goLeft loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")
;;


let goRight loc = match loc with
LOC(l, TOP) -> raise (NOMOVE "right of top")
| LOC(l, HAND(left, up, t::right)) -> LOC(t, HAND(l::left, up, right))
| LOC(l, HAND(left, up, [])) -> raise (NOMOVE "right of first")
;;

let goUp loc = match loc with
 LOC(d, TOP) -> raise (NOMOVE "top of top")
| LOC(d, HAND(left, up, right)) -> 
        let upper = NODE ((List.rev left)@[d]@right) in
        LOC(upper, up)
;;
let goDown loc = match loc with
LOC(NODE [], _) -> raise (NOMOVE "empty node")
| LOC(LEAF i, down) -> raise (NOMOVE "down of leaf")
| LOC(NODE(l::sons), down) -> LOC(l, HAND([], down, sons))
;;
