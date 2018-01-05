exception NOMOVE of string
type item = string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
|LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
|LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goDown loc =
		match loc with
		| LOC(LEAF x,_) -> raise (NOMOVE "down of bottom")
		| LOC((NODE (h::t)), TOP)-> LOC(h, HAND([],TOP, t))
		| LOC((NODE (h::t)), HAND(left,up,right) ) -> LOC(h,HAND([],HAND(left,up,right),t))
let goUp loc = 
		match loc with
		| LOC(t,TOP) -> raise (NOMOVE "up of top")
		| LOC(t,HAND(l2,HAND(left,up,right),r2)) -> LOC(NODE (List.append (List.append l2 [t]) r2) ,HAND(left,up,right)) 
		| LOC(t,HAND(l2,TOP,r2)) ->  
		 LOC(NODE (List.append (List.append l2 [t]) r2) ,TOP) 

