exception NOMOVE of string
type item=string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
let goLeft loc = match loc with
LOC(t, TOP) -> raise( NOMOVE( "left of top"))
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE( "left of first"))
let goRight loc=
	match loc with 
		| LOC(t, TOP) ->raise(NOMOVE("Right of TOP"))
		| LOC(t, HAND(l, z, [])) ->raise(NOMOVE("Right of first"))
		| LOC(t, HAND(l, z, right::r))->LOC(right, HAND(t::l, z, r))
let  goUp loc=
	match loc with
		| LOC(t, TOP) ->raise (NOMOVE("Up of TOP"))
		| LOC(t, HAND(_, TOP,_))->raise (NOMOVE("Up of First"))
		| LOC(t,HAND(l, z, r))->LOC(NODE(  (List.rev l)@(t::r) ), z)
let goDown loc=
	match loc with
		| LOC(LEAF(s), _) ->raise (NOMOVE("Down of Leaf"))
		| LOC(NODE(h::t), z)->LOC(h, HAND([],z, t))
		| LOC(NODE([]), z)->raise(NOMOVE("Wrong Location"))
