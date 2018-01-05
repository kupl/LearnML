exception NOMOVE of string

type item = string
type tree = LEAF of item
|NODE of tree list
type zipper = TOP
|HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
|LOC(t,TOP) -> raise (NOMOVE "cannot move")
|LOC(t,HAND(l::left,up,right)) -> LOC(l,HAND(left,up,t::right))
|LOC(t,HAND([],up,right)) -> raise (NOMOVE "cannot move")

let goRight loc = match loc with
|LOC(t,TOP) -> raise (NOMOVE "cannot move")
|LOC(t,HAND(left,up,r::right)) -> LOC(r,HAND(t::left,up,right))
|LOC(t,HAND(left,up,[])) -> raise (NOMOVE "cannot move")

let rec goUp loc = match loc with
|LOC(t,TOP) -> raise (NOMOVE "cannot move")
|LOC(t,HAND([],zip,right)) -> LOC(NODE(t::right),zip)
|LOC(t,HAND(l::left,zip,right)) -> goUp(LOC(l,HAND(left,zip,t::right)))

let goDown loc = match loc with
|LOC(NODE(d::t),zip) -> LOC(d,HAND([],zip,t))
|LOC(LEAF(item),zip) -> raise (NOMOVE "cannot move")
|LOC(NODE([]),zip) -> raise (NOMOVE "cannot move")
