exception NOMOVE of string

type tree = LEAF of string
  		| NODE of tree list
type zipper = TOP
  		| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper;;


let goLeft loc = match loc with
			LOC(t,TOP) -> raise (NOMOVE("left of top"))
			| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
			| LOC(t, HAND([],up,right)) -> raise (NOMOVE("left of first"))

let goRight loc = match loc with
			LOC(t,TOP) -> raise (NOMOVE("right of top"))
			| LOC(t, HAND(left,up,r::right)) -> LOC(r, HAND(t::left,up,right))
			| LOC(t, HAND(left,up,[])) -> raise (NOMOVE("right of first"))

let goUp loc = match loc with
			LOC(t,TOP) -> raise (NOMOVE("top of top"))
			| LOC(t, HAND(left,up,right)) -> (match up with
								HAND(subleft,subup,subright) -> LOC(NODE (left@(t::right)), HAND(subleft,subup,subright))
								| _ -> raise (NOMOVE("top of top"))
							);;

let goDown loc = match loc with
			LOC(t,z) -> ( match t with
					LEAF x -> raise (NOMOVE("down of down"))
					| NODE l -> (match l with
							x::sublist -> LOC(x,HAND([],z,sublist))
							| _ -> raise (NOMOVE("down of down"))
						)
					);;