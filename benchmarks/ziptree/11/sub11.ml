(* PL HW1-8 "짚-짚-나무"
   2007-11738
   알렉산더 *)

type item = string
exception NOMOVE of string

(* 나무 구조 *)
type tree = LEAF of item
          | NODE of tree list

(* zipper *)
type zipper = TOP
            | HAND of tree list * zipper * tree list

(* location *)
type location = LOC of tree * zipper

(* go LEFT *)
let goLeft loc =
    match loc with
        LOC (t, TOP) -> raise (NOMOVE "left of top")
      | LOC (t, HAND(l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
      | LOC (t, HAND([], up, right)) -> raise (NOMOVE "left of first")

(* go RIGHT *)
let goRight loc =
    match loc with
        LOC (t, TOP) -> raise (NOMOVE "right of top")
      | LOC (t, HAND(left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
      | LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

(* go UP *)
let goUp loc =
    match loc with
        LOC (t, TOP) -> raise (NOMOVE "now location is top")
      | LOC (t, HAND(left, up, right)) -> LOC (NODE ((List.rev left) @ (t::right)), up)

(* go DOWN *)
let goDown loc =
    match loc with
        LOC (LEAF l, _) -> raise (NOMOVE "now location is leaf")
      | LOC (NODE (leaf::treelist), zip) -> LOC (leaf, HAND([], zip, treelist))
      | LOC (NODE [], _) -> raise (NOMOVE "now location is leaf")



(* =========================================================================
(* Test *)
(* second "*" *)
let loc1 =
    LOC (LEAF "*",
        HAND([LEAF "c"],
             HAND([LEAF "+"; NODE[LEAF "a"; LEAF "*"; LEAF "b"]],
                  TOP,
                  []),
             [LEAF "d"]
        )
    )

(* up of loc1 *)
let loc12 =
    LOC (NODE[LEAF "c"; LEAF"*"; LEAF "d"],
        HAND([LEAF "+"; NODE[LEAF "a"; LEAF "*"; LEAF "b"]],
             TOP,
             []
        )
    )

(* "b" *)
let loc2 =
    LOC (LEAF "b",
        HAND([LEAF "*"; LEAF "a"],
             HAND([],
                  TOP,
                  [LEAF "+"; NODE[LEAF "c"; LEAF "*"; LEAF "d"]]),
             []
        )
    )

*)
