(*2016-11690*)
type item = string

type tree = LEAF of item
	| NODE of tree list

type zipper = TOP
	| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc = (*https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=4972*)
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left,up,right)) -> LOC(NODE(List.rev_append left (t::right)),up) (*list should be appended reversely*)(* @ : same operation with List.append *)

let goDown loc = 
	match loc with
	| LOC(LEAF key, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "down of empty node")  
	| LOC(NODE (l::children), zipper) -> LOC(l, HAND([],zipper,children))

	(* move of empty node exception?! https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=4996*)

	
(* test code

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
;; *)