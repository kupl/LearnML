exception NOMOVE of string

type item = string
type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first");;

let goRight = (fun loc -> match loc with
| LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
);;

let goUp = (fun loc -> match loc with
| LOC(t, TOP) -> raise (NOMOVE "up of top")
| LOC(t, HAND(l, z, r)) -> 
	let x = (List.rev l)@[t]@r in 
	(match z with 
	| TOP -> LOC(NODE x, TOP)
	| HAND (l, z, r) -> LOC((NODE x), HAND(l, z, r))
  )
);;

let goDown = (fun loc -> match loc with
| LOC(LEAF a, z) -> raise (NOMOVE "down of leaf")
| LOC(NODE [], z) -> raise (NOMOVE "treelist is empty")
| LOC(NODE (h::t), z) -> LOC(h, HAND([], z, t))
);;

