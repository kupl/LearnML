(* Computer Science/2005-11759/Sangcheol Park/Exercise 2-4.*)

type item = string;;
type tree = LEAF of item | NODE of tree list;;
type zipper = TOP | HAND of tree list * zipper * tree list;;
type location = LOC of tree * zipper;;

exception NOMOVE of string;;

let goLeft loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l:: left, up, right)) -> LOC(l, HAND(left, up, t:: right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
;;

let goRight loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r :: right)) -> LOC(r, HAND(t :: left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
;;

let goUp loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> LOC (NODE (List.append left (t::right)), up)
;;

let goDown loc = match loc with
	| LOC(LEAF l, z) -> raise (NOMOVE "down of leaf")
	| LOC(NODE (hd::lst), z) -> LOC (hd, HAND([], z,lst)) 
	| LOC(NODE [], z) -> raise (NOMOVE "down of leaf")
;;

(* 

let ex_tree = NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"];
    LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]];;

let ex_loc = LOC (LEAF "*",
        HAND([LEAF "c"],
            HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
                TOP,
                []),
            [LEAF "d"]));;

let left = [LEAF "a"; LEAF "b"];;
let middle = LEAF "t";;
let right = [LEAF "c"; LEAF "d"];;
middle::right;;
left::middle::right;;
List.append left (middle::right);;
NODE (List.append left (middle::right));;

goLeft ex_loc;;
goRight ex_loc;;
goLeft (goRight ex_loc);;
goRight (goLeft ex_loc);;
goUp ex_loc;;
goUp(goUp ex_loc);;
goUp(goUp(goUp ex_loc));;
goDown(goUp(goUp ex_loc));;
goDown ex_loc;;
goRight (goDown (goUp ex_loc));;
*)