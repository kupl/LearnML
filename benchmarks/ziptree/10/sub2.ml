(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-8 *)

exception NOMOVE of string

type tree = LEAF of item
	| NODE of tree list
	and item = string

type zipper = TOP | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
	| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of last")
(*
let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, HAND(l::up_left, up, []), right)) -> LOC (l, HAND(up_left, up, [NODE (List.concat [List.rev left; t::right])] ))
	| LOC(t, HAND(left, HAND([], up, l::up_right), right)) -> LOC (l, HAND([NODE (List.concat [List.rev left; t::right])], up, up_right))
	| LOC(t, HAND(left, up, right)) -> raise (NOMOVE "illegal tree")
*)
let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "illegal location")
	| LOC(t, HAND(left, TOP, right)) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, HAND(up_left, up, up_right), right)) -> LOC (List.hd ((List.rev up_left)@ ((NODE ((List.rev left)@ (t::right))) :: up_right)), HAND ([], up, List.tl (List.rev up_left@((NODE (List.rev left@(t::right))) :: up_right))))

let goDown loc = match loc with
	LOC (LEAF x, _) -> raise (NOMOVE "left? right?")
	| LOC (NODE x, z) -> if List.length x < 1 then raise (NOMOVE "illegal tree")
						else LOC (List.hd x, HAND([], z, List.tl x))

(*
(* test code *)
let tree0 = NODE	[
			NODE	[LEAF "a"; LEAF "*"; LEAF "b"];
			LEAF	"+";
			NODE	[LEAF "c"; LEAF "*"; LEAF "d"]
			]

let loc0 = LOC		(
			LEAF "*",
			HAND	(
				[LEAF "c"],
				HAND	(
						[
							LEAF "+";
							NODE [LEAF "a"; LEAF "*"; LEAF "b"]
						],
						TOP,
						[]
					),
				[LEAF "d"]
				)
			)
*)
