type item = string
type tree =
    LEAF of item
    | NODE of tree list

type zipper =
    TOP
    | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
    LOC(t,TOP) -> raise(NOMOVE "left of top")
    | LOC(t, HAND(l::left,up,right)) -> LOC(l,HAND(left,up,t::right))
    | LOC(t, HAND([],up,right)) -> raise(NOMOVE "left of first")
;;

let goRight loc = match loc with
    LOC(t,TOP) -> raise(NOMOVE "right of top")
    | LOC(t, HAND(left,up,r::right)) -> LOC(r,HAND(t::left,up,right))
    | LOC(t, HAND(left,up,[])) -> raise(NOMOVE "right of last")
;;

let goUp loc = match loc with
    LOC(t,TOP) -> raise(NOMOVE "up of top")
    | LOC(t, HAND(left,up,right)) ->
            let r = t::right in
            LOC(NODE(List.rev_append left r),up)
;;

let goDown loc = match loc with
    LOC(LEAF(_),_) -> raise(NOMOVE "down of leaf")
    | LOC(NODE([]),HAND(_,_,_)) -> raise(NOMOVE "no child")
    | LOC(NODE(head::tail),zip) -> LOC(head,HAND([],zip,tail))
    | LOC(NODE([]),TOP) -> raise(NOMOVE "no child")
;;
