type item = string
type tree = LEAF of item | NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc:location) :location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp (loc:location) :location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "top of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE ((List.rev left)@(t::right)), up)

let sub loc = 
  match loc with
  |LOC (t,zip) -> (t,zip)
  |_ -> raise(NOMOVE "")

let goDown (loc:location) :location =
  let (t,zip) = sub loc in
  match t with
  | LEAF _ -> raise (NOMOVE "at bottom")
  | NODE (l::other) -> LOC(l, HAND([], zip, other))
  | NODE [] -> raise (NOMOVE "it's a weird tree")

(* 
let tl = LOC(LEAF "*", 
             HAND([LEAF "c"], 
                  HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], 
			           TOP, 
					   []), 
			      [LEAF "d"])) 
*)
