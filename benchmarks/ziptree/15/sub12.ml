exception NOMOVE of string

type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc =
	match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")
(*
let rec merge_to_left (tr: tree) (zl: tree list) : tree =
	match tr with
	| LEAF _ -> merge_to_left (NODE (tr::[])) zl
	| NODE l -> (
		match zl with
		| [] -> NODE l
		| h::[] -> NODE (h::l)
		| h::t -> merge_to_left (NODE (h::l)) t)

let rec merge_to_right (tr: tree) (zr: tree list) : tree =
	match tr with
	| LEAF _ -> merge_to_right (NODE (tr::[])) zr
	| NODE l -> (
		match zr with
		| [] -> NODE l
		| h::[] -> NODE l)
*)
let goUp loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> (
		let merge_with_left = List.rev (t::left) in
		let merge_with_right = merge_with_left @ right in
		LOC(NODE merge_with_right, up)
	)

let goDown loc =
	match loc with
	| LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "down of empty node")
	| LOC(NODE (h::l), zip) -> LOC (h, HAND([], zip, l))
(*
let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; 
                      LEAF "+"; 
                      NODE [LEAF "c"; LEAF "*"; LEAF "d"]], 
                TOP) 

let (|>) g f = f g 

let a91 = loc1 |> goDown 
let a92 = loc1 |> goDown |> goDown 
let a93 = loc1 |> goDown |> goUp |> goDown 
let a94 = loc1 |> goDown |> goDown |> goRight 
let a95 = loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight 
let a96 = loc1 |> goDown |> goRight |> goRight |> goDown |> goRight 
let a97 = 
    try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true 
;;
*)