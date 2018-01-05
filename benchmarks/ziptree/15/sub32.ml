(* C:\Users\saigoy\Desktop\zip-zip-tree.ml *)

type item = string;;

type tree = LEAF of item
| NODE of tree list;;

type zipper = TOP
| HAND of tree list * zipper * tree list;;

type location = LOC of tree * zipper;;

exception NOMOVE of string;;

let goLeft loc = 
match loc with
| LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right));;

let goRight loc = 
match loc with
| LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of last")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right));;

let rec listConcat : 'a list * 'a list ->  'a list = fun (list1, list2) -> 
match list1 with
| [] -> list2
| hd::tl -> ( hd :: (listConcat (tl, list2) ) );;

let goUp loc = 
match loc with
| LOC(t, TOP) -> raise (NOMOVE "up of top")
| LOC(t, HAND(left, up, right)) -> LOC(NODE(listConcat(left, t::right)), up);;

let goDown loc = 
match loc with
| LOC(LEAF t, zip) -> raise(NOMOVE "down of leaf")
| LOC(NODE [], zip) -> raise(NOMOVE "down of empty list")
| LOC(NODE (hd::tl), zip) ->  LOC(hd, HAND([], zip, tl));;

