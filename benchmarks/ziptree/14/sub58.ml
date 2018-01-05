(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 3
 * CSE / 2012-13456 / Gao, Chengbin *)

exception NOMOVE of string

type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = 
    match loc with 
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
                 (* the left most in list 'left' is the right most *)
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = 
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "right of too")
    | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = 
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
    | LOC(t, HAND(left, up, right)) ->
            LOC(NODE((List.rev left) @ [t] @ right), up)

let goDown loc = 
    match loc with
    | LOC(LEAF _, z) -> raise (NOMOVE "down of leaf")
    | LOC(NODE lvs, z) ->
            match lvs with
            | [] -> raise (NOMOVE "down of empty node")
              (* move the left most?? *)
            | hd::tl -> LOC(hd, HAND([], z, tl))

