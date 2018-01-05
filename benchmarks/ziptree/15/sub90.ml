type item = string
type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper


exception NOMOVE of string

let goLeft loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")


let goRight loc = match loc with
LOC(t,TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")


let goUp loc = match loc with
| LOC(t,TOP) -> raise (NOMOVE "up of TOP")
| LOC(t,HAND(left, up, right)) -> (match left, right with
                                   |h1::t1, h2::t2 -> if t1 = [] && t2 = [] then LOC(NODE([h1;t;h2]),up)
                                                       else if t1 = [] then LOC(NODE([h1]@[t]@right),up)
                                                      else if t2 = [] then LOC(NODE(left@[t]@[h2]),up)
                                                      else LOC(NODE(left@[t]@right),up) 
                                   |h1::t1,[] -> if t1 = [] then LOC(NODE([h1;t]),up)
                                                 else LOC(NODE(left@[t]),up)
                                   |[],h2::t2 -> if t2 = [] then LOC(NODE([t;h2]),up)
                                                 else LOC(NODE([t]@right),up)
                                   |[],[] -> LOC(NODE([t]),up)
                                
                                   )


let goDown loc = match loc with
| LOC(NODE([]),c) -> raise (NOMOVE "down of BOTTOM")
| LOC(NODE(a::b),c) -> LOC(a, HAND([], c, b))
| LOC(LEAF x, c) -> raise (NOMOVE "down of BOTTOM")


