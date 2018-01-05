(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 4: zip-zip-tree
 * Written by Dongho Kang 
 *)

type item = string 

type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string 

let goLeft loc = 
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
;;

let goRight: location -> location = fun loc ->
    match loc with 
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first") 
;;

let goUp: location -> location = fun loc ->
    match loc with 
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
    | LOC(t, HAND(left, up, right)) -> LOC(NODE(left @ [t] @ right), up)
;;

let goDown: location -> location = fun loc -> 
    match loc with 
    | LOC(LEAF i, z) -> raise (NOMOVE "down of leaf")
    | LOC(NODE t_list, z) -> LOC(List.hd t_list, HAND([], z, List.tl t_list))
;;

