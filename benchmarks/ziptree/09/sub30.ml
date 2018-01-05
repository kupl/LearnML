(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 4
 *)

type item = string

type tree = LEAF of item
	    | NODE of tree list

type zipper = TOP
	      | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft (loc:location) =
  match loc with
      LOC (t, TOP) -> raise (NOMOVE "left of top")
    | LOC (t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC (t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc:location) =
  match loc with
      LOC (t, TOP) -> raise (NOMOVE "right of top")
    | LOC (t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp (loc:location) =
  match loc with
      LOC (t, TOP) -> raise (NOMOVE "up of top")
    | LOC (t, HAND(left, TOP, right)) -> LOC((NODE((List.rev left) @ [t] @ right)), TOP)
    | LOC (t, HAND(left, HAND(up_left, up_up, up_right), right)) -> LOC((NODE((List.rev left) @ [t] @ right)), HAND(up_left, up_up, up_right))

let goDown (loc:location) =
  match loc with
      LOC (LEAF _, _) -> raise (NOMOVE "down of leaf")
    | LOC (NODE (t::tl), z) -> LOC(t, HAND([], z, tl))
    | LOC (NODE [], _) -> raise (NOMOVE "non-child node")


(*
;;
let t = NODE [ NODE [ LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [ LEAF "c"; LEAF "*"; LEAF "d"]];;
let l = LOC (t, TOP);;

let except1 f arg1 =
  try
    let _ = f (arg1) in
      assert (false)
  with _ -> assert(true);;

except1 goRight l;;
except1 goLeft l;;
except1 goUp l;;

let l = goDown (l);;

except1 goLeft l;;

let l = goRight (l);;
let l = goRight (l);;

except1 goRight l;;

let l = goDown (l);;

except1 goDown l;;

let l = goUp (l);;
let l = goUp (l);;
*)
