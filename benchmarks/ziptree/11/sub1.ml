(* 2004-11951 Noh, Soon Hyun *)

type item = string
exception NOMOVE of string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

(* example function *)
let goLeft loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) ->
		LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right))
		-> raise (NOMOVE "left of first")

(* almost same as goRight *)
let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) ->
		LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, []))
		-> raise (NOMOVE "right of first")

let goUp loc =
	match loc with
	(* Exception on TOP case *)
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	(* used List internal fuction for implement *)
	| LOC(t, HAND(left, up, right))
		(* left should be inverted *)
		-> LOC((NODE (List.append (List.rev left) 
					  (List.append [t] right)))
		      , up)

let goDown loc =
	match loc with
	(* Exception on LEAF(bottom) case *)
	| LOC(LEAF l, z) -> raise (NOMOVE "down of bottom")
	| LOC(NODE (l::l'), z)
		-> LOC(l, HAND([], z, l'))
	| LOC(NODE [], z) -> raise (NOMOVE "NODE [] detected")
(*
let test1 = LOC (LEAF "*",
		 HAND([LEAF "c"],
		       HAND  ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
			       TOP,
			       []),
		      [LEAF "d"]))
let tree1 =
NODE
[NODE
[NODE [LEAF "a"; LEAF "/"; LEAF "b"]; LEAF "-";
NODE [NODE [LEAF "c"; LEAF "*"; LEAF "d"]; LEAF "+"; LEAF "e"]];
LEAF "%"; LEAF "F"]

let printLocation loc =
	match loc with
	| LOC(LEAF l, _) -> print_string l; print_char '\n'
	| _ -> raise (NOMOVE "not unique node")

let _ = printLocation (goDown (goLeft (goDown (goUp (goUp (goUp test1))))))
*)
