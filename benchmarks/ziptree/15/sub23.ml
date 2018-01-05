(* 생명과학부 / 2011-10915 / 신지민 / Homework 2-4 *)

type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
      LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight : location -> location = fun loc ->
	match loc with
	|LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(left,up,r::right)) -> LOC(r, HAND(t::left,up,right))
	|LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp: location -> location = fun loc ->
	match loc with 
	|LOC(t, TOP) -> raise (NOMOVE "up of top")
        |LOC(t, HAND(left,up,right)) -> LOC(NODE((List.rev left)@(t::right)),up)

let goDown: location -> location = fun loc ->
	match loc with
	|LOC(LEAF i, z) -> raise (NOMOVE "down of leaf")
	|LOC(NODE(l::tl),z) -> LOC(l,HAND([],z,tl))
	|LOC(NODE [],z) -> raise (NOMOVE "empty node")



(*
let loc = LOC(LEAF "*", HAND([LEAF "c"],
			HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
				TOP,
				[]),
			[LEAF "d"]))


let d = goUp loc
let r = goUp d
let k = goDown r



let rec print_node = fun node ->
	match node with
	|LEAF i -> print_endline(i)
	|NODE [] -> print_endline("empty node")
	|NODE(l::tl) -> begin
			let _= print_node(l) in
			print_node(NODE(tl))
			end

let rec print_zipper = fun z ->
	match z with
	|HAND(l,z,r) -> begin
		let _= print_endline("This is left tree") in
		let _= print_node(NODE(l)) in
		let _= print_zipper(z) in
		let _= print_endline("This is right tree") in
		print_node(NODE(r)) 
			end
	|TOP -> print_endline("TOP")

let rec print_loc = fun loc ->
	match loc with
	|LOC(t,z) -> begin
			let _= print_endline("This is current tree") in
			let _= print_node(t) in
			print_zipper(z)
		     end		

let _= print_loc d
let _= print_endline("----------")
let _= print_loc r
let _= print_endline("_-------")
let _= print_loc k 
*)
