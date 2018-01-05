type item = string
type tree = LEAF of item
| NODE of tree list
 exception NOMOVE of string
type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
 | LOC(t, TOP) -> raise (NOMOVE "left of top")
 | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
 | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

 let goRight loc = match loc with
 | LOC(t, TOP) -> raise (NOMOVE "right of top")
 | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
 | LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

 let goDown loc = match loc with
 | LOC(NODE(l::t), zipper) -> LOC(l, HAND([], zipper, t)) 
 | LOC(LEAF l, zipper) -> raise(NOMOVE "error on goDown")  
 | LOC(NODE[], TOP) -> raise (NOMOVE "error on goDown") 
 
 let goUp loc = match loc with
 | LOC(t, TOP) -> raise (NOMOVE "error on goUp")
 | LOC(t, HAND(left, up, right)) -> LOC(NODE(t::right), up)
