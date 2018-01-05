(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 4 *)

(* exceptions *)
exception NOMOVE of string

(* types *)
type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

(* goLeft *)
let goLeft loc =
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

(* goRight *)
let goRight loc =
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

(* goUp *)
let goUp loc =
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "parent of top")
    | LOC(t, HAND(left, up, right)) ->
            LOC(NODE((List.rev left)@[t]@right), up)

(* goDown *)
let goDown loc =
    match loc with
    | LOC(LEAF _, _) -> raise (NOMOVE "child of leaf")
    | LOC(NODE([]), _) -> raise (NOMOVE "child of heirless")
    | LOC(NODE(e::rest), zipper) -> LOC(e, HAND([], zipper, rest))
