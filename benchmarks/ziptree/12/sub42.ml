type item = string
type tree = LEAF of item
			| NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string 

let goLeft loc = 
	match loc with
		  LOC(t, TOP) -> raise (NOMOVE "left of top")
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")
let goRight loc =
	match loc with
		| LOC (t, TOP) -> raise (NOMOVE "right of top")
		| LOC (t, HAND (left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
		| LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of last")
let goUp loc =
	match loc with
		| LOC (t, TOP) -> raise (NOMOVE "up of top")
		| LOC (t, HAND (left, up, right)) -> LOC (NODE ((List.rev (t::left))@right), up)
let goDown loc =
	match loc with
		| LOC (LEAF x, _) -> raise (NOMOVE "down of bottom")
		| LOC (NODE [], _) -> raise (NOMOVE "down of empty")
		| LOC (NODE (l::l_), z) -> LOC (l, HAND ([], z, l_))

(* TEST SET *)
(*
goRight( LOC(LEAF "*", HAND ([LEAF "e"],HAND ([LEAF "-"], HAND([NODE [NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]]], TOP, []), []), [LEAF "f"])));;
goUp(LOC (LEAF "f", HAND ([LEAF "*"; LEAF "e"], HAND ([LEAF "-"], HAND([NODE [NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]]], TOP, []), []), [])));;
goLeft (goUp (LOC (NODE [LEAF "e"; LEAF "*"; LEAF "f"], HAND ([LEAF "-"], HAND([NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]], TOP, []), []))));;
goDown (LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP));;
goRight(goRight(LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))));;
goRight(goRight(LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))));;
*)