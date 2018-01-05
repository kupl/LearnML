(*start*)
exception NOMOVE of string

type item = string
type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
(*
let rec inverse_tree_list(tl : tree list) : tree list =
match tl with
| [] -> []
| hd::tl -> (inverse_tree_list(tl))::(hd::[])
*)
let goRight(loc:location) : location =
match loc with
|LOC(t, TOP) -> raise (NOMOVE "right of top")
|LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left,up,right))
|LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp(loc: location) : location =
match loc with
|LOC(t, TOP) -> raise (NOMOVE "up of top")
|LOC(t, HAND(left, up, right)) -> LOC(NODE(List.append (List.rev(t::left)) right),up)

let goDown(loc: location) : location = 
match loc with
|LOC(LEAF(x),_) ->raise (NOMOVE "down of leaf")
|LOC(NODE(hd::tl), hand) -> LOC(hd, HAND([], hand, tl))
|LOC(NODE([]), _) -> raise (NOMOVE "down of empty node")
(*end*)
