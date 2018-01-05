type tree = LEAF of item
	  | NODE of tree list
and item = string
type zipper = TOP
	    | HAND of tree list * zipper * tree list
type loc = LOC of tree * zipper

exception NOMOVE of string


(* reverse : 'a list -> 'a list *)
let rec reverse lst = 
 match lst with
  h::t -> (reverse t) @ [h]
  |[] -> []
;;





let goLeft loc = 
 match loc with
  LOC (t, TOP) -> raise (NOMOVE "left of top")
  |LOC (t, HAND (target::left, up, right)) -> (LOC (target, HAND (left, up, t::right)))
  |LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")
;;


let goRight loc = 
 match loc with
  LOC (t, TOP) -> raise (NOMOVE "right of top")
  |LOC (t, HAND (left, up, target::right)) -> (LOC (target, HAND (t::left, up, right)))
  |LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of last")
;;


let goUp loc = 
 match loc with
  LOC (t, TOP) -> raise (NOMOVE "top of top")
  |LOC (t, HAND (left, up, right)) -> (LOC ((NODE ((List.rev left) @ [t] @ right)),
 				           up))
;;


let goDown loc = 
 match loc with
  LOC (LEAF _, _) -> raise (NOMOVE "down of leaf")
  |LOC (NODE (h::t), up) -> (LOC (h, HAND ([], up, t)))
  |LOC (NODE _, _) -> raise (NOMOVE "down of empty node")
;;
