

exception NOMOVE of string 

type tree = LEAF of string
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
LOC(t, TOP) -> (raise NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> (raise NOMOVE "left of first"  )
    
let goRight loc =
    match loc with
    LOC(t, TOP) -> (raise NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left,up,[])) -> (raise NOMOVE "right of last")

let goUp loc =
    match loc with
    LOC(t,TOP) -> (raise NOMOVE "up of top")
    |LOC(t, HAND(left, a, right)) -> LOC(NODE(left@t::right),a)
   
let goDown loc =
    match loc with 
    LOC(LEAF a, _) -> (raise NOMOVE "down of bottom")
    |LOC(NODE(l::t), a) -> LOC(l, HAND([], a, t))    
    |_ -> (raise NOMOVE "")

    
