(* ex9 *)
type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
	
let goRight loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let sub loc = 
  match loc with
  |LOC(tr,zip) -> (tr,zip)
  |_ -> raise (NOMOVE "")


let goDown loc = 
  let (tr,zip) = sub loc in
	match tr with
	  LEAF str -> raise (NOMOVE "down of bottom")
	| NODE [] -> raise (NOMOVE "empty tree")
	| NODE (a::lst) -> LOC(a, HAND([], zip, lst))


let goUp loc = 
  let (tr,zip) = sub loc in
	match zip with
	  TOP -> raise (NOMOVE "up of top")
	| HAND (left, up, right) -> LOC (NODE (List.append left (tr::right)), up)
