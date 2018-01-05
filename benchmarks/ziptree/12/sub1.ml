exception NOMOVE of string;;

type item = string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper;;

let goRight loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left,up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(_, _, [])) -> raise (NOMOVE "right of first");;
let goUp loc =
	match loc with
	| LOC(_, TOP)-> raise (NOMOVE "up is top")
	| LOC(t, HAND(left,up,right)) -> LOC(NODE (List.append (List.rev left)  (t::right)), up);;

let goDown loc = 
	match loc with
	| LOC(LEAF _, _) -> raise (NOMOVE "Now is leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "Now is empty NODE")
	| LOC(NODE (h::t), z) -> LOC(h, HAND([], z, t));;