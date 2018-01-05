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
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

(*
let a =LOC (LEAF "*",
 	HAND([LEAF "c"], 
 		HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], 
 			TOP, 
 			[]),
		[LEAF "d"]))

let b = LOC (LEAF "c",
 HAND ([], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []),
  [LEAF "*"; LEAF "d"]))

let c =LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], 
	HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], 
	TOP, 
	[])) 
let d =LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], 
	TOP) 
*)

let goRight loc = 
	match loc with 
	| LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC (t, HAND(left, up ,[])) -> raise (NOMOVE "right of first")

let goUp loc =
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "up of top")
	| LOC (t, HAND(left, TOP, right)) -> LOC(NODE((List.rev left)@[t]@right), TOP)
	| LOC (t, HAND(left, up, right)) -> LOC(NODE((List.rev left)@ [t]@ right), up)

let goDown loc =
	match loc with
	| LOC (LEAF t, _) -> raise (NOMOVE "down of leaf")
	| LOC (NODE (a::b), TOP) -> LOC(a, HAND([],TOP,b))
	| LOC (NODE (a::b), c) -> LOC(a, HAND([],c,b))
	| LOC (NODE [], _) -> raise (NOMOVE "down of nothing")

