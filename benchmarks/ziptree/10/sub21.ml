exception NOMOVE of string
type tree = LEAF of item
 	    | NODE of tree list
	    and item = string 
type zipper = TOP
	    | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper 

let goLeft loc =  
    match loc with 
    LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
    ;;
let goRight loc = 
    match loc with 
    LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND([t] @ left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
   ;;
let goUp loc = 
    match loc with 
    LOC(t, TOP) -> raise (NOMOVE "up of TOP, can't go up!")
    | LOC(t, HAND(left, up, right)) -> LOC(NODE(List.rev left @ [t] @ right), up)
    ;;
let goDown loc =  
    match loc with
    LOC(LEAF t, zip) -> raise (NOMOVE "down of leaf, can't go down!")
    | LOC(NODE(t), TOP) -> LOC(List.hd t, (HAND([], TOP, List.tl t)))
    | LOC(NODE(t), HAND(left, up, right)) -> LOC(List.hd t, (HAND([], HAND(left, up, right), List.tl t)))
    ;;
