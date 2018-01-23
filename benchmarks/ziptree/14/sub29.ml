(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 2 - 3 *)
type item = string
type tree = LEAF of item
	| NODE of tree list
type zipper = TOP
	| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let rec merge arg = match arg with
	| ([], lis) -> lis
	| (t::lessadd, lis) -> t::(merge(lessadd, lis))

let goLeft loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
	
let goRight loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
	
let goUp loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "top of top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(merge(left,t::right)), up)
	
let goDown loc = match loc with
	| LOC(LEAF t, x) -> raise (NOMOVE "down of leaf")
	| LOC(NODE[], x) -> raise (NOMOVE "node without descendant")
	| LOC(NODE(l::t), z) -> LOC(l, HAND([], z, t))
