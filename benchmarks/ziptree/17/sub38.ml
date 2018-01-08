type item = string
type tree = LEAF of item
	| NODE of tree list
type zipper = TOP
	| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft: location -> location = fun (loc) ->
	match loc with
	LOC (t, TOP) -> raise (NOMOVE "left of top")
	| LOC (t, HAND (l::left, up, right)) -> LOC (l, HAND(left, up, t::right))
	| LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")
let goRight: location -> location = fun (loc) ->
	match loc with
	LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND (left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
	| LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of first")

let myappend: 'a list * 'a * 'alist -> 'alist = fun (l, t, r) ->
	List.append (List.rev l) (t::r)
let myhd = List.hd
let mytl = List.tl
let goUp: location -> location = fun (loc) ->
	match loc with
	LOC (t, TOP) -> raise (NOMOVE "up of top")
	| LOC (t, HAND (left, up, right)) -> LOC (NODE (myappend (left, t, right)), up)
let goDown: location -> location = fun (loc) ->
	match loc with
	LOC (LEAF item, _) -> raise (NOMOVE "down of leaf")
	| LOC (NODE [], _) -> raise (NOMOVE "down of empty node")
	| LOC (NODE tlist, cur) -> LOC (myhd tlist, HAND ([], cur, mytl tlist))