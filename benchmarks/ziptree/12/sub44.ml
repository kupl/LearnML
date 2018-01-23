exception NOMOVE

type item = string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = 
	match loc with
		LOC(t, TOP) -> raise (NOMOVE )
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise NOMOVE 
;;

let goRight loc = 
	match loc with
		LOC(t, TOP) -> raise (NOMOVE )
		| LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
		| LOC(t, HAND(left,up,[])) -> raise NOMOVE
;;

let goUp loc = 
	match loc with
		LOC(t, TOP) -> raise (NOMOVE)
		| LOC(t, HAND(left, HAND(t1,t2,t3), right)) -> LOC((NODE(List.rev(left)@[t]@right)), HAND(t1, t2, t3))
		| LOC(t ,HAND(left, TOP, right)) -> raise(NOMOVE)
;;

let goDown loc = 
	match loc with
	|LOC(NODE(t::d),hnd) -> LOC(t,HAND([],hnd,d))
	|LOC(LEAF _,_) -> raise (NOMOVE)
	|_ -> raise (NOMOVE)
;;
