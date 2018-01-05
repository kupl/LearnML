type item = string
type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let rec mergelist : (tree list * tree list) -> tree list = fun (x, y) ->
 match x with
 | [] -> y
 | h::t -> mergelist (t, h::y)

let goLeft loc = match loc with
 LOC(t, TOP) -> raise (NOMOVE "left of top")
 | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
 | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
 LOC(t, TOP) -> raise (NOMOVE "right of top")
 | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
 | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
 LOC(t, TOP) -> raise (NOMOVE "top of top")
 | LOC(t, HAND(left, up, right)) -> LOC(NODE (mergelist (left, t::right)), up)

let goDown loc = match loc with
 LOC(LEAF x, zip) -> raise (NOMOVE "down of leaf")
 | LOC(NODE ([]), zip) -> raise (NOMOVE "down of null")
 | LOC(NODE (left::right), zip) -> LOC(left, HAND([], zip, right))