(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 23, 2014
 *)

(* Exercise 3 : Zip Zip Tree *)

type item = string
type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let print_tree t =
let print_space w = print_string (String.make w '.') in
let rec print_tree_sub w = function
| LEAF item -> print_space w; print_string item; print_string "\n"
| NODE nodes -> List.iter (fun t -> (print_tree_sub (w+2) t); () ) nodes
in (print_tree_sub 0 t)

exception NOMOVE of string

let goLeft: location -> location = fun loc -> match loc with
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(_, TOP)          -> raise (NOMOVE "left of top")
| LOC(_, HAND([],_,_)) -> raise (NOMOVE "left of first")

let goRight: location -> location = fun loc -> match loc with
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(_, TOP)          -> raise (NOMOVE "right of top")
| LOC(_, HAND(_,_,[])) -> raise (NOMOVE "right of last")

let goUp: location -> location    = fun loc -> match loc with
LOC(_, TOP) -> raise (NOMOVE "top of top")
| LOC(l, HAND([], up, c::r)) -> LOC(NODE (l::c::r), up)
| LOC(r, HAND(l::c, up, [])) -> LOC(NODE ((l::c)@[r]), up)
| LOC(_, HAND([], up, [])) -> LOC(NODE [], up)
| LOC(_, HAND(_::_, _, _::_)) -> raise (NOMOVE "no more move to up")

let goDown: location -> location  = fun loc -> match loc with
| LOC(NODE (l::r), up) -> LOC(l, HAND([], up, r))
| LOC(LEAF _, _)  -> raise (NOMOVE "no more move to down")
| LOC(NODE [], _) -> raise (NOMOVE "no more move to down")
