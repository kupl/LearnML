(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercise 5
*)

type item = string
type tree = LEAF of item
					| NODE of tree list
type zipper = TOP
						| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let rec listrev((input: tree list), (output: tree list)): tree list =
	match input with
	| [] -> output
	| (hd::tl) -> listrev(tl, hd::output)

let goLeft loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(listrev(left, [])@(t::right)), up)
	| LOC(t, HAND(left, TOP, right)) -> raise (NOMOVE "up of first")

let goDown loc = 
	match loc with
	| LOC(LEAF l, HAND(left, up, right)) -> raise (NOMOVE "down of top")
	| LOC(NODE (hd::tl), zip) -> LOC(hd, HAND([], zip, tl))

