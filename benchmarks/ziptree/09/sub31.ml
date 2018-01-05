(* School of Computer Science & Engineering
 * 2009-23151
 * Sungkeun Cho
 * HW 2 - Exercise 4
 *)

type item = string;;

type tree = LEAF of item
	    | NODE of tree list;;

type zipper = TOP
	      | HAND of tree list * zipper * tree list;;

type location = LOC of tree * zipper;;

exception NOMOVE of string;;


let goLeft loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first");;

let goRight loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first");;

let goUp loc = 
  let rec l2r l r =
    match l with
	[] -> r
      | hd::tl -> l2r tl (hd::r)
  in
    match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
      | LOC(t, HAND(left, up, right)) -> 
	  LOC(
	    NODE (l2r left (t::right)),
	    up
	  )
;;


let goDown loc =
  match loc with
      LOC(LEAF(_),_) -> raise (NOMOVE "down of LEAF")
    | LOC(NODE([]),_) -> raise (NOMOVE "empty NODE")
    | LOC(NODE(hd::tl),z) -> 
	LOC(hd,HAND([],z,tl))
;;



(*
let star2 = LOC (LEAF "*",
		 HAND([LEAF "c"],
		      HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
			   TOP,
			   []),
		      [LEAF "d"]));;

goLeft(star2);;
goLeft(goLeft(star2));;
goRight(star2);;
goRight(goRight(star2));;
goDown(star2);;
goUp(star2);;
goUp(goUp(star2));;
goUp(goUp(goUp(star2)));;
goRight(goDown(goLeft(goLeft(goUp(star2)))));;
*)

