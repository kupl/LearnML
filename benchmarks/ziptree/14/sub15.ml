(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 3  *)

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

let goRight loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(_,_,[])) -> raise (NOMOVE "right of last")

let goUp loc =
    let rec mergeLeft left right =
        match left with
        | [] -> right
        | l::rest -> mergeLeft rest (l::right)
    in
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
    | LOC(t, HAND(left, up, right)) -> LOC(NODE(mergeLeft left (t::right)), up)

let goDown loc =
    match loc with
    | LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
    | LOC(NODE [], _) -> raise (NOMOVE "down of empty node")
    | LOC(NODE (leftNode :: siblings), z) -> LOC(leftNode, HAND([], z, siblings))
